// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stddef.h>
#include <stdio.h>

#include "julia.h"
#include "julia_gcext.h"

// Comparing pointers in C without triggering undefined behavior
// can be difficult. As the GC already assumes that the memory
// range goes from 0 to 2^k-1 (region tables), we simply convert
// to uintptr_t and compare those.

static inline int cmp_ptr(void *p, void *q)
{
    uintptr_t paddr = (uintptr_t)p;
    uintptr_t qaddr = (uintptr_t)q;
    if (paddr < qaddr)
        return -1;
    else if (paddr > qaddr)
        return 1;
    else
        return 0;
}

static inline int lt_ptr(void *a, void *b)
{
    return (uintptr_t)a < (uintptr_t)b;
}

#if 0
static inline int gt_ptr(void * a, void * b)
{
    return (uintptr_t)a > (uintptr_t)b;
}

static inline void *max_ptr(void *a, void *b)
{
    if ((uintptr_t) a > (uintptr_t) b)
        return a;
    else
        return b;
}

static inline void *min_ptr(void *a, void *b)
{
    if ((uintptr_t) a < (uintptr_t) b)
        return a;
    else
        return b;
}
#endif

/* align pointer to full word if mis-aligned */
static inline void *align_ptr(void *p)
{
    uintptr_t u = (uintptr_t)p;
    u &= ~(sizeof(p) - 1);
    return (void *)u;
}

// We use treaps -- a form of balanced trees -- to be able to
// find allocations based on their address.

typedef struct treap_t {
    struct treap_t *left, *right;
    size_t prio;
    void *addr;
    size_t size;
} treap_t;

static treap_t *treap_free_list;

treap_t *alloc_treap(void)
{
    treap_t *result;
    if (treap_free_list) {
        result = treap_free_list;
        treap_free_list = treap_free_list->right;
    }
    else
        result = malloc(sizeof(treap_t));
    result->left = NULL;
    result->right = NULL;
    result->addr = NULL;
    result->size = 0;
    return result;
}

void free_treap(treap_t *t)
{
    t->right = treap_free_list;
    treap_free_list = t;
}

static inline int test_bigval_range(treap_t *node, void *p)
{
    char *l = node->addr;
    char *r = l + node->size;
    if (lt_ptr(p, l))
        return -1;
    if (!lt_ptr(p, r))
        return 1;
    return 0;
}

#define L(t) ((t)->left)
#define R(t) ((t)->right)

static inline void treap_rot_right(treap_t **treap)
{
    /*       t                 l       */
    /*     /   \             /   \     */
    /*    l     r    -->    a     t    */
    /*   / \                     / \   */
    /*  a   b                   b   r  */
    treap_t *t = *treap;
    treap_t *l = L(t);
    treap_t *a = L(l);
    treap_t *b = R(l);
    L(l) = a;
    R(l) = t;
    L(t) = b;
    *treap = l;
}

static inline void treap_rot_left(treap_t **treap)
{
    /*     t                   r       */
    /*   /   \               /   \     */
    /*  l     r    -->      t     b    */
    /*       / \           / \         */
    /*      a   b         l   a        */
    treap_t *t = *treap;
    treap_t *r = R(t);
    treap_t *a = L(r);
    treap_t *b = R(r);
    L(r) = t;
    R(r) = b;
    R(t) = a;
    *treap = r;
}

static treap_t *treap_find(treap_t *treap, void *p)
{
    while (treap) {
        int c = test_bigval_range(treap, p);
        if (c == 0)
            return treap;
        else if (c < 0)
            treap = L(treap);
        else
            treap = R(treap);
    }
    return NULL;
}

static void treap_insert(treap_t **treap, treap_t *val)
{
    treap_t *t = *treap;
    if (t == NULL) {
        L(val) = NULL;
        R(val) = NULL;
        *treap = val;
    }
    else {
        int c = cmp_ptr(val->addr, t->addr);
        if (c < 0) {
            treap_insert(&L(t), val);
            if (L(t)->prio > t->prio) {
                treap_rot_right(treap);
            }
        }
        else if (c > 0) {
            treap_insert(&R(t), val);
            if (R(t)->prio > t->prio) {
                treap_rot_left(treap);
            }
        }
    }
}

static void treap_delete_node(treap_t **treap)
{
    for (;;) {
        treap_t *t = *treap;
        if (L(t) == NULL) {
            *treap = R(t);
            free_treap(t);
            break;
        }
        else if (R(t) == NULL) {
            *treap = L(t);
            free_treap(t);
            break;
        }
        else {
            if (L(t)->prio > R(t)->prio) {
                treap_rot_right(treap);
                treap = &R(*treap);
            }
            else {
                treap_rot_left(treap);
                treap = &L(*treap);
            }
        }
    }
}

static int treap_delete(treap_t **treap, void *addr)
{
    while (*treap != NULL) {
        int c = cmp_ptr(addr, (*treap)->addr);
        if (c == 0) {
            treap_delete_node(treap);
            return 1;
        }
        else if (c < 0) {
            treap = &L(*treap);
        }
        else {
            treap = &R(*treap);
        }
    }
    return 0;
}

static uint64_t xorshift_rng_state = 1;

static uint64_t xorshift_rng(void)
{
    uint64_t x = xorshift_rng_state;
    x = x ^ (x >> 12);
    x = x ^ (x << 25);
    x = x ^ (x >> 27);
    xorshift_rng_state = x;
    return x * (uint64_t)0x2545F4914F6CDD1DUL;
}

static treap_t *bigvals;
static size_t bigval_startoffset;

// Hooks to allocate and free external objects (bigval_t's).

void alloc_bigval(void *addr, size_t size)
{
    treap_t *node = alloc_treap();
    node->addr = addr;
    node->size = size;
    node->prio = xorshift_rng();
    treap_insert(&bigvals, node);
}

void free_bigval(void *p)
{
    if (p) {
        treap_delete(&bigvals, p);
    }
}

// Auxiliary roots. These serve no special purpose, except
// allowing us to verify that root scanning works.

#define NAUXROOTS 1024
static jl_value_t *aux_roots[NAUXROOTS];
JL_DLLEXPORT size_t gc_counter_full, gc_counter_inc;

JL_DLLEXPORT jl_value_t *get_aux_root(size_t n)
{
    if (n >= NAUXROOTS)
        jl_error("get_aux_root: index out of range");
    return aux_roots[n];
}

JL_DLLEXPORT void set_aux_root(size_t n, jl_value_t *val)
{
    if (n >= NAUXROOTS)
        jl_error("set_aux_root: index out of range");
    aux_roots[n] = val;
}

JL_DLLEXPORT size_t get_gc_counter(int full)
{
    if (full)
        return gc_counter_full;
    else
        return gc_counter_inc;
}

static size_t obj_sweeps = 0;

JL_DLLEXPORT size_t get_obj_sweeps()
{
    return obj_sweeps;
}

typedef struct {
    size_t size;
    size_t capacity;
    jl_value_t *data[1];
} dynstack_t;

static jl_datatype_t *datatype_stack_internal;
static jl_datatype_t *datatype_stack_external;
static jl_datatype_t *datatype_stack;
static jl_ptls_t ptls;

static size_t gc_alloc_size(jl_value_t *val)
{
    size_t size;
    if (jl_typeis(val, datatype_stack))
        size = sizeof(jl_value_t *);
    else if (jl_typeis(val, datatype_stack_internal) || jl_typeis(val, datatype_stack_external))
        size = offsetof(dynstack_t, data) +
                ((dynstack_t *)val)->capacity * sizeof(jl_value_t *);
    else if (jl_typeis(val, jl_string_type)) {
        size = jl_string_len(val) + sizeof(size_t) + 1;
        // Round up to whole word size
        if (size % sizeof(void *) != 0)
            size += sizeof(void *) - size % sizeof(void *);
    }
    else
        size = 0;
    return size;
}

JL_DLLEXPORT int internal_obj_scan(jl_value_t *val)
{
    if (jl_gc_is_internal_obj_alloc(val)) {
        if (jl_gc_internal_obj_base_ptr(val) != val)
            return 0;
        size_t size = gc_alloc_size(val);
        char *addr = (char *)val;
        for (size_t i = 0; i <= size; i++) {
            if (jl_gc_internal_obj_base_ptr(addr + i) != val)
                return 0;
        }
        return 1;
    }
    else {
        treap_t *node = treap_find(bigvals, val);
        if (!node)
            return 0;
        char *addr = node->addr;
        if ((jl_value_t *)addr != val)
            return 0;
        size_t size = node->size;
        for (size_t i = 0; i <= size; i++) {
            if (treap_find(bigvals, addr + i) != node)
                return 0;
        }
        return 1;
    }
}

dynstack_t *allocate_stack_mem(size_t capacity)
{
    size_t size = offsetof(dynstack_t, data) + capacity * sizeof(jl_value_t *);
    jl_datatype_t *type = datatype_stack_internal;
    if (size > jl_gc_max_internal_obj_size())
        type = datatype_stack_external;
    dynstack_t *result = (dynstack_t *)jl_gc_alloc_typed(ptls, size, type);
    result->size = 0;
    result->capacity = capacity;
    return result;
}

void check_stack(const char *name, jl_value_t *p)
{
    if (jl_typeis(p, datatype_stack))
        return;
    jl_type_error(name, (jl_value_t *)datatype_stack, p);
}

void check_stack_notempty(const char *name, jl_value_t *p)
{
    check_stack(name, p);
    dynstack_t *stk = *(dynstack_t **)p;
    if (stk->size == 0)
        jl_errorf("%s: dynstack_t empty", name);
}

// Stacks use double indirection in order to be resizable.
// The outer object is a single word containing a pointer to
// a `dynstack_t`, which can contain a variable number of
// Julia objects; the `capacity` field denotes the number of objects
// that can be stored without resizing storage, the `size` field
// denotes the actual number of objects. GC scanning should ignore
// any storage past those.

// Create a new stack object

JL_DLLEXPORT jl_value_t *stk_make()
{
    jl_value_t *hdr =
            jl_gc_alloc_typed(ptls, sizeof(jl_value_t *), datatype_stack);
    JL_GC_PUSH1(hdr);
    *(dynstack_t **)hdr = NULL;
    dynstack_t *stk = allocate_stack_mem(8);
    *(dynstack_t **)hdr = stk;
    jl_gc_schedule_foreign_sweepfunc(ptls, (jl_value_t *)(stk));
    JL_GC_POP();
    return hdr;
}

// Return a pointer to the inner `dynstack_t` struct.

JL_DLLEXPORT jl_value_t *stk_blob(jl_value_t *s)
{
    return (jl_value_t *)(*(dynstack_t **)s);
}

// Push `v` on `s`.

JL_DLLEXPORT void stk_push(jl_value_t *s, jl_value_t *v)
{
    check_stack("push", s);
    dynstack_t *stk = *(dynstack_t **)s;
    if (stk->size < stk->capacity) {
        stk->data[stk->size++] = v;
        jl_gc_wb((jl_value_t *)stk, v);
    }
    else {
        dynstack_t *newstk = allocate_stack_mem(stk->capacity * 3 / 2 + 1);
        newstk->size = stk->size;
        memcpy(newstk->data, stk->data, sizeof(jl_value_t *) * stk->size);
        *(dynstack_t **)s = newstk;
        newstk->data[newstk->size++] = v;
        jl_gc_schedule_foreign_sweepfunc(ptls, (jl_value_t *)(newstk));
        jl_gc_wb_back((jl_value_t *)newstk);
        jl_gc_wb(s, (jl_value_t *)newstk);
    }
}

// Return top value from `s`. Raise error if not empty.

JL_DLLEXPORT jl_value_t *stk_top(jl_value_t *s)
{
    check_stack_notempty("top", s);
    dynstack_t *stk = *(dynstack_t **)s;
    return stk->data[stk->size - 1];
}

// Pop a value from `s` and return it. Raise error if not empty.

JL_DLLEXPORT jl_value_t *stk_pop(jl_value_t *s)
{
    check_stack_notempty("pop", s);
    dynstack_t *stk = *(dynstack_t **)s;
    stk->size--;
    return stk->data[stk->size];
}

// Number of objects on the stack.

JL_DLLEXPORT size_t stk_size(jl_value_t *s)
{
    check_stack("empty", s);
    dynstack_t *stk = *(dynstack_t **)s;
    return stk->size;
}

static jl_module_t *module;

// Mark auxiliary roots.

void root_scanner(int full)
{
    for (int i = 0; i < NAUXROOTS; i++) {
        if (aux_roots[i])
            jl_gc_mark_queue_obj(ptls, aux_roots[i]);
    }
}

// Hooks to run before and after GC.
//
// As a simple example, we only track counters for full
// and partial collections.

void pre_gc_func(int full)
{
    if (full)
        gc_counter_full++;
    else
        gc_counter_inc++;
}

void post_gc_func(int full) {}

// Mark the outer stack object (containing only a pointer to the data).

uintptr_t mark_stack(jl_ptls_t ptls, jl_value_t *p)
{
    if (!*(void **)p)
        return 0;
    return jl_gc_mark_queue_obj(ptls, *(jl_value_t **)p) != 0;
}

// Mark the actual stack data.
// This is used both for `StackData` and `StackDataLarge`.

uintptr_t mark_stack_data(jl_ptls_t ptls, jl_value_t *p)
{
    dynstack_t *stk = (dynstack_t *)p;
    // Alternate between two marking approaches for testing so
    // that we test both.
    if (gc_counter_full & 1) {
        jl_gc_mark_queue_objarray(ptls, p, stk->data, stk->size);
        return 0;
    }
    else {
      uintptr_t n = 0;
      for (size_t i = 0; i < stk->size; i++) {
          if (jl_gc_mark_queue_obj(ptls, stk->data[i]))
              n++;
      }
      return n;
    }
}

void sweep_stack_data(jl_value_t *p)
{
    obj_sweeps++;
    dynstack_t *stk = (dynstack_t *)p;
    if (stk->size > stk->capacity)
        jl_error("internal error during sweeping");
}

// Safely execute Julia code

jl_value_t *checked_eval_string(const char *code)
{
    jl_value_t *result = jl_eval_string(code);
    if (jl_exception_occurred()) {
        // none of these allocate, so a gc-root (JL_GC_PUSH) is not necessary
        jl_call2(
                jl_get_function(jl_base_module, "showerror"),
                jl_stderr_obj(),
                jl_exception_occurred());
        jl_printf(jl_stderr_stream(), "\n");
        jl_atexit_hook(1);
        exit(1);
    }
    assert(result && "Missing return value but no exception occurred!");
    return result;
}

void abort_with_error(int full)
{
    abort();
}

int main()
{
    // Install callbacks. This should happen before `jl_init()` and
    // before any GC is called.

    jl_gc_set_cb_notify_external_alloc(alloc_bigval, 1);
    jl_gc_set_cb_notify_external_free(free_bigval, 1);

    jl_gc_enable_conservative_gc_support();
    jl_init();
    ptls = jl_get_ptls_states();
    jl_gc_set_cb_root_scanner(root_scanner, 1);
    jl_gc_set_cb_pre_gc(pre_gc_func, 1);
    jl_gc_set_cb_post_gc(post_gc_func, 1);
    // Test that deregistration works
    jl_gc_set_cb_root_scanner(abort_with_error, 1);
    jl_gc_set_cb_root_scanner(abort_with_error, 0);
    // Create module to store types in.
    module = jl_new_module(jl_symbol("TestGCExt"));
    module->parent = jl_main_module;
    jl_set_const(jl_main_module, jl_symbol("TestGCExt"), (jl_value_t *)module);
    // Define Julia types for our stack implementation.
    datatype_stack = jl_new_foreign_type(
            jl_symbol("Stack"), module, jl_any_type, mark_stack, NULL, 1, 0);
    jl_set_const(module, jl_symbol("Stack"), (jl_value_t *)datatype_stack);
    datatype_stack_internal = jl_new_foreign_type(
            jl_symbol("StackData"),
            module,
            jl_any_type,
            mark_stack_data,
            sweep_stack_data,
            1,
            0);
    jl_set_const(
            module,
            jl_symbol("StackData"),
            (jl_value_t *)datatype_stack_internal);
    datatype_stack_external = jl_new_foreign_type(
            jl_symbol("StackDataLarge"),
            module,
            jl_any_type,
            mark_stack_data,
            sweep_stack_data,
            1,
            1);
    jl_set_const(
            module,
            jl_symbol("StackDataLarge"),
            (jl_value_t *)datatype_stack_external);
    // Remember the offset of external objects
    bigval_startoffset = jl_gc_external_obj_hdr_size();
    // Run the actual tests
    checked_eval_string(
            "let dir = dirname(unsafe_string(Base.JLOptions().julia_bin))\n"
            // disable the package manager
            "    ENV[\"JULIA_PKGDIR\"] = joinpath(dir, \"disabled\")\n"
            // locate files relative to the "embedding" executable
            "    stdlib = filter(env -> startswith(Base.find_package(Base, "
            "\"Distributed\"), env), Base.load_path())[end]\n"
            "    push!(empty!(LOAD_PATH), dir, stdlib)\n"
            "end");

    checked_eval_string("import LocalTest");
}
