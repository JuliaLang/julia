// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  runtime support for extensible enum types

  An enum type is a primitive-like DataType (isprimitivetype is set, so
  bitcast/reinterpret/unbox work unchanged) whose typename carries a member
  table `enumtab` and has the `isenumtype` flag set. The table is an svec with
  a 3-slot header (storage type, isopen::Bool, next-auto-value hint::UInt64)
  followed by 5 slots per member: (name::Symbol, owning module::Module,
  canonical boxed instance, isexplicit::Bool, identity hash::UInt64). The
  identity hash is computed from the owning module's name chain and the member
  name, making `hash` of enum values independent of their (rebasable) bit
  patterns and therefore stable across sessions.

  Member identity is (enum type, owning module, member name). The table is
  copy-on-write: readers take an acquire-load snapshot; writers hold
  world_counter_lock (which member registration needs anyway, to declare the
  member as a constant binding in its owning module).

  Auto-assigned member values are session-dependent: they are chosen as the
  next free bit pattern at registration time and are *rebased* when a package
  image is loaded (see staticdata.c), keyed on member identity. Explicitly
  assigned values are stable; two members may never share a value.
*/

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// Log of all enum member registrations made while generating output
// (precompile); consumed by staticdata.c to build the member list that a
// package image re-registers at load time. Flat groups of 5:
// (enumtype, module, name, instance, isexplicit).
jl_array_t *jl_enum_member_log JL_GLOBALLY_ROOTED = NULL;

#define ENUM_TAB_HEADER 3
#define ENUM_TAB_STRIDE 5

static jl_svec_t *enum_tab(jl_datatype_t *et)
{
    if (!jl_is_enumtype((jl_value_t*)et))
        jl_errorf("%s is not an enum type", jl_symbol_name(et->name->name));
    jl_svec_t *tab = jl_atomic_load_acquire(&et->name->enumtab);
    assert(tab != NULL);
    return tab;
}

static size_t enum_tab_nmembers(jl_svec_t *tab) JL_NOTSAFEPOINT
{
    return (jl_svec_len(tab) - ENUM_TAB_HEADER) / ENUM_TAB_STRIDE;
}

static jl_datatype_t *enum_tab_storagetype(jl_svec_t *tab) JL_NOTSAFEPOINT
{
    return (jl_datatype_t*)jl_svecref(tab, 0);
}

static int enum_tab_isopen(jl_svec_t *tab) JL_NOTSAFEPOINT
{
    return jl_svecref(tab, 1) == jl_true;
}

static uint64_t enum_read_raw_bits(const void *p, size_t nbytes) JL_NOTSAFEPOINT
{
    switch (nbytes) {
    case 1: { uint8_t b;  memcpy(&b, p, 1); return b; }
    case 2: { uint16_t b; memcpy(&b, p, 2); return b; }
    case 4: { uint32_t b; memcpy(&b, p, 4); return b; }
    default: { uint64_t b; memcpy(&b, p, 8); return b; }
    }
}

static uint64_t enum_read_bits(jl_value_t *v, size_t nbytes) JL_NOTSAFEPOINT
{
    return enum_read_raw_bits(jl_data_ptr(v), nbytes);
}

static jl_value_t *enum_box_bits(jl_datatype_t *t, uint64_t bits, size_t nbytes)
{
    uint8_t buf[8];
    switch (nbytes) {
    case 1: { uint8_t b = (uint8_t)bits;   memcpy(buf, &b, 1); break; }
    case 2: { uint16_t b = (uint16_t)bits; memcpy(buf, &b, 2); break; }
    case 4: { uint32_t b = (uint32_t)bits; memcpy(buf, &b, 4); break; }
    default: memcpy(buf, &bits, 8); break;
    }
    return jl_new_bits((jl_value_t*)t, buf);
}

// index into tab of the member (mod, name), or (size_t)-1
static size_t enum_find_member(jl_svec_t *tab, jl_module_t *mod, jl_sym_t *name) JL_NOTSAFEPOINT
{
    size_t n = enum_tab_nmembers(tab);
    for (size_t i = 0; i < n; i++) {
        size_t k = ENUM_TAB_HEADER + i * ENUM_TAB_STRIDE;
        if (jl_svecref(tab, k) == (jl_value_t*)name && jl_svecref(tab, k + 1) == (jl_value_t*)mod)
            return k;
    }
    return (size_t)-1;
}

// index into tab of the member holding the value `bits`, or (size_t)-1
static size_t enum_find_value(jl_svec_t *tab, uint64_t bits, size_t nbytes) JL_NOTSAFEPOINT
{
    size_t n = enum_tab_nmembers(tab);
    for (size_t i = 0; i < n; i++) {
        size_t k = ENUM_TAB_HEADER + i * ENUM_TAB_STRIDE;
        if (enum_read_bits(jl_svecref(tab, k + 2), nbytes) == bits)
            return k;
    }
    return (size_t)-1;
}

// smallest free bit pattern at or after the hint (wrapping around the storage
// domain); errors if the enum is full
static uint64_t enum_next_free(jl_svec_t *tab, jl_datatype_t *et, size_t nbytes)
{
    uint64_t domain_max = nbytes >= 8 ? UINT64_MAX : (((uint64_t)1 << (8 * nbytes)) - 1);
    if (nbytes < 8 && enum_tab_nmembers(tab) > domain_max)
        jl_errorf("enum type %s is full", jl_symbol_name(et->name->name));
    uint64_t hint = jl_unbox_uint64(jl_svecref(tab, 2));
    uint64_t candidate = hint;
    for (;;) {
        if (candidate <= domain_max && enum_find_value(tab, candidate, nbytes) == (size_t)-1)
            return candidate;
        if (candidate >= domain_max) {
            if (hint == 0)
                jl_errorf("enum type %s is full", jl_symbol_name(et->name->name));
            candidate = 0;
            domain_max = hint - 1; // scanned [hint, max]; now scan [0, hint)
            hint = 0;
            continue;
        }
        candidate++;
    }
}

// Deterministic, session-independent hash of the member identity
// (owning module, name): combines the symbol hashes of the member name and
// the owning module's name chain. Symbol hashes depend only on the symbol's
// string, so this is stable across sessions, unlike the member's (rebasable)
// bit pattern.
static uint64_t enum_member_hash(jl_module_t *mod, jl_sym_t *name) JL_NOTSAFEPOINT
{
    uintptr_t h = name->hash;
    jl_module_t *m = mod;
    while (1) {
        h = bitmix(h, m->name->hash);
        if (m->parent == m || m->parent == NULL)
            break;
        m = m->parent;
    }
    return (uint64_t)inthash(h);
}

// copy-on-write append of a member (and, for auto assignment, the updated
// next-auto hint `newhint`) to et's member table; caller holds
// world_counter_lock and keeps `tab` (the current table) and `instance` rooted
static void enum_tab_append(jl_datatype_t *et, jl_svec_t *tab, jl_module_t *mod,
                            jl_sym_t *name, jl_value_t *instance, int isexplicit,
                            uint64_t newhint)
{
    jl_value_t *hintbox = NULL;
    jl_value_t *hashbox = NULL;
    jl_svec_t *newtab = NULL;
    JL_GC_PUSH3(&hintbox, &hashbox, &newtab);
    if (newhint != 0)
        hintbox = jl_box_uint64(newhint);
    hashbox = jl_box_uint64(enum_member_hash(mod, name));
    // no allocations may happen while the new table is partially initialized
    size_t oldlen = jl_svec_len(tab);
    newtab = jl_alloc_svec_uninit(oldlen + ENUM_TAB_STRIDE);
    for (size_t i = 0; i < oldlen; i++)
        jl_svecset(newtab, i, jl_svecref(tab, i));
    if (hintbox != NULL)
        jl_svecset(newtab, 2, hintbox);
    jl_svecset(newtab, oldlen + 0, (jl_value_t*)name);
    jl_svecset(newtab, oldlen + 1, (jl_value_t*)mod);
    jl_svecset(newtab, oldlen + 2, instance);
    jl_svecset(newtab, oldlen + 3, isexplicit ? jl_true : jl_false);
    jl_svecset(newtab, oldlen + 4, hashbox);
    jl_atomic_store_release(&et->name->enumtab, newtab);
    jl_gc_wb(et->name, newtab);
    JL_GC_POP();
}

// Convert an explicit member value to the storage type's bit pattern: any
// primitive Integer value is accepted as long as it is representable.
static uint64_t enum_convert_value(jl_datatype_t *et, jl_datatype_t *storagetype,
                                   size_t nbytes, jl_sym_t *name, jl_value_t *value)
{
    jl_value_t *vt = jl_typeof(value);
    if (vt == (jl_value_t*)storagetype)
        return enum_read_bits(value, nbytes);
    jl_value_t *integer_type = jl_get_global(jl_core_module, jl_symbol("Integer"));
    if (!jl_is_primitivetype(vt) || jl_is_enumtype(vt) || jl_datatype_size(vt) > 8 ||
        integer_type == NULL || !jl_subtype(vt, integer_type))
        jl_errorf("invalid value for member %s of enum type %s: expected an integer of at most 8 bytes",
                  jl_symbol_name(name), jl_symbol_name(et->name->name));
    size_t vnb = jl_datatype_size(vt);
    uint64_t raw = enum_read_bits(value, vnb);
    jl_value_t *signed_type = jl_get_global(jl_core_module, jl_symbol("Signed"));
    int src_signed = signed_type != NULL && jl_subtype(vt, signed_type);
    int dst_signed = signed_type != NULL && jl_subtype((jl_value_t*)storagetype, signed_type);
    uint64_t uval = raw; // source value, sign-extended to 64 bits if signed
    if (src_signed && vnb < 8) {
        uint64_t signbit = (uint64_t)1 << (8 * vnb - 1);
        if (raw & signbit)
            uval = raw | ~((((uint64_t)1 << (8 * vnb)) - 1));
    }
    int64_t sval = (int64_t)uval;
    int src_negative = src_signed && sval < 0;
    int fits;
    if (dst_signed) {
        int64_t dmin = nbytes == 8 ? INT64_MIN : -((int64_t)1 << (8 * nbytes - 1));
        int64_t dmax = nbytes == 8 ? INT64_MAX : ((int64_t)1 << (8 * nbytes - 1)) - 1;
        fits = src_negative ? sval >= dmin : uval <= (uint64_t)dmax;
    }
    else {
        uint64_t dmax = nbytes == 8 ? UINT64_MAX : (((uint64_t)1 << (8 * nbytes)) - 1);
        fits = !src_negative && uval <= dmax;
    }
    if (!fits)
        jl_errorf("invalid value for member %s of enum type %s: value is not representable in storage type %s",
                  jl_symbol_name(name), jl_symbol_name(et->name->name),
                  jl_symbol_name(storagetype->name->name));
    return nbytes == 8 ? uval : (uval & ((((uint64_t)1 << (8 * nbytes)) - 1)));
}

static void enum_log_member(jl_datatype_t *et, jl_module_t *mod, jl_sym_t *name,
                            jl_value_t *instance, int isexplicit)
{
    if (!jl_generating_output())
        return;
    if (jl_enum_member_log == NULL)
        jl_enum_member_log = jl_alloc_vec_any(0);
    jl_array_ptr_1d_push(jl_enum_member_log, (jl_value_t*)et);
    jl_array_ptr_1d_push(jl_enum_member_log, (jl_value_t*)mod);
    jl_array_ptr_1d_push(jl_enum_member_log, (jl_value_t*)name);
    jl_array_ptr_1d_push(jl_enum_member_log, instance);
    jl_array_ptr_1d_push(jl_enum_member_log, isexplicit ? jl_true : jl_false);
}

// Register the member (mod, name) in the enum type `et`, declare it as a
// constant binding in `mod`, and return its canonical boxed instance.
// `value` is an instance of the storage type, or NULL for automatic
// assignment. Re-registering an existing member is idempotent (auto, or
// explicit with equal bits); an explicit value that differs from the existing
// member's, or that is already taken by another member, is an error.
JL_DLLEXPORT jl_value_t *jl_enum_add_member(jl_datatype_t *et, jl_module_t *mod,
                                            jl_sym_t *name, jl_value_t *value)
{
    jl_svec_t *tab = enum_tab(et);
    jl_datatype_t *storagetype = enum_tab_storagetype(tab);
    size_t nbytes = jl_datatype_size(storagetype);
    uint64_t explicit_bits = 0;
    if (value != NULL)
        explicit_bits = enum_convert_value(et, storagetype, nbytes, name, value);

    jl_value_t *instance = NULL;
    JL_GC_PUSH2(&tab, &instance);
    JL_LOCK(&world_counter_lock);
    // re-load under the lock; the snapshot above was only for pre-validation
    tab = jl_atomic_load_relaxed(&et->name->enumtab);
    size_t k = enum_find_member(tab, mod, name);
    if (k != (size_t)-1) {
        // idempotent re-registration
        instance = jl_svecref(tab, k + 2);
        if (value != NULL && explicit_bits != enum_read_bits(instance, nbytes))
            jl_errorf("enum member %s.%s of %s already has value %llu",
                      jl_symbol_name(mod->name), jl_symbol_name(name),
                      jl_symbol_name(et->name->name),
                      (unsigned long long)enum_read_bits(instance, nbytes));
        JL_UNLOCK(&world_counter_lock);
        JL_GC_POP();
        return instance;
    }
    if (mod != et->name->module && !enum_tab_isopen(tab))
        jl_errorf("cannot add member %s to enum type %s: it is not extensible",
                  jl_symbol_name(name), jl_symbol_name(et->name->name));
    uint64_t bits;
    if (value != NULL) {
        bits = explicit_bits;
        size_t taken = enum_find_value(tab, bits, nbytes);
        if (taken != (size_t)-1) {
            jl_module_t *othermod = (jl_module_t*)jl_svecref(tab, taken + 1);
            jl_errorf("enum value %llu for member %s of %s is already taken by %s.%s",
                      (unsigned long long)bits, jl_symbol_name(name),
                      jl_symbol_name(et->name->name), jl_symbol_name(othermod->name),
                      jl_symbol_name((jl_sym_t*)jl_svecref(tab, taken)));
        }
    }
    else {
        bits = enum_next_free(tab, et, nbytes);
    }
    instance = enum_box_bits(et, bits, nbytes);
    enum_tab_append(et, tab, mod, name, instance, /*isexplicit*/value != NULL,
                    /*newhint*/value == NULL ? bits + 1 : 0);

    enum_log_member(et, mod, name, instance, value != NULL);

    // declare `const name = instance` in the owning module (same pattern as
    // jl_declare_constant_val2)
    size_t new_world = jl_atomic_load_relaxed(&jl_world_counter) + 1;
    jl_binding_partition_t *bpart =
        jl_declare_constant_val3(NULL, mod, name, instance, PARTITION_KIND_CONST, new_world);
    if (jl_atomic_load_relaxed(&bpart->min_world) == new_world)
        jl_atomic_store_release(&jl_world_counter, new_world);
    JL_UNLOCK(&world_counter_lock);
    JL_GC_POP();
    return instance;
}

// Validate an extension declaration: the enum must be extensible and the
// (re-declared) storage type must match.
JL_DLLEXPORT void jl_enum_extend_check(jl_datatype_t *et, jl_module_t *mod,
                                       jl_datatype_t *storagetype)
{
    jl_svec_t *tab = enum_tab(et);
    if (!enum_tab_isopen(tab))
        jl_errorf("cannot extend enum type %s: it is not extensible",
                  jl_symbol_name(et->name->name));
    if ((jl_value_t*)storagetype != jl_svecref(tab, 0))
        jl_errorf("storage type %s in extension of enum type %s does not match its storage type %s",
                  jl_symbol_name(storagetype->name->name), jl_symbol_name(et->name->name),
                  jl_symbol_name(enum_tab_storagetype(tab)->name->name));
    (void)mod;
}

// current member-table snapshot (including the 3-slot header)
JL_DLLEXPORT jl_svec_t *jl_enum_members(jl_datatype_t *et)
{
    return enum_tab(et);
}

// storage type of an enum type
JL_DLLEXPORT jl_value_t *jl_enum_storagetype(jl_datatype_t *et)
{
    return jl_svecref(enum_tab(et), 0);
}

// whether an enum type is extensible
JL_DLLEXPORT int jl_enum_isopen(jl_datatype_t *et)
{
    return enum_tab_isopen(enum_tab(et));
}

// member holding the same bits as the enum instance `x`, as
// svec(name, module, instance, isexplicit, identityhash), or nothing if the
// bit pattern does not correspond to a registered member
JL_DLLEXPORT jl_value_t *jl_enum_lookup_value(jl_value_t *x)
{
    jl_datatype_t *et = (jl_datatype_t*)jl_typeof(x);
    jl_svec_t *tab = enum_tab(et);
    size_t nbytes = jl_datatype_size(enum_tab_storagetype(tab));
    size_t k = enum_find_value(tab, enum_read_bits(x, nbytes), nbytes);
    if (k == (size_t)-1)
        return jl_nothing;
    JL_GC_PUSH1(&tab);
    jl_value_t *r = (jl_value_t*)jl_svec(5, jl_svecref(tab, k), jl_svecref(tab, k + 1),
                                         jl_svecref(tab, k + 2), jl_svecref(tab, k + 3),
                                         jl_svecref(tab, k + 4));
    JL_GC_POP();
    return r;
}

// Whether the bits at `ptr`, interpreted as an instance of `bt` (which must
// be pointer-free), are stable across sessions. Bit patterns of auto-assigned
// enum members are not: they are rebased when a package image is loaded, so
// they must not be baked into precompiled artifacts. Explicit member values
// and bit patterns that match no member are stable.
static int enum_bits_stable(jl_datatype_t *bt, const char *ptr) JL_NOTSAFEPOINT
{
    if (jl_is_enumtype((jl_value_t*)bt)) {
        jl_svec_t *tab = jl_atomic_load_acquire(&bt->name->enumtab);
        size_t nbytes = jl_datatype_size(enum_tab_storagetype(tab));
        size_t k = enum_find_value(tab, enum_read_raw_bits(ptr, nbytes), nbytes);
        return k == (size_t)-1 || jl_svecref(tab, k + 3) == jl_true;
    }
    if (bt->layout == NULL)
        return 1;
    size_t nf = jl_datatype_nfields(bt);
    for (size_t i = 0; i < nf; i++) {
        if (jl_field_isptr(bt, i))
            continue;
        jl_value_t *ft = jl_field_type_concrete(bt, i);
        const char *fptr = ptr + jl_field_offset(bt, i);
        if (jl_is_uniontype(ft)) {
            size_t fsz = jl_field_size(bt, i);
            uint8_t sel = ((const uint8_t*)fptr)[fsz - 1];
            jl_value_t *active = jl_nth_union_component(ft, sel);
            if (active != NULL && jl_is_datatype(active) &&
                !enum_bits_stable((jl_datatype_t*)active, fptr))
                return 0;
        }
        else if (jl_is_datatype(ft) && !enum_bits_stable((jl_datatype_t*)ft, fptr)) {
            return 0;
        }
    }
    return 1;
}

// Whether the value `v` (including enum-typed bits anywhere inside it) is
// safe to bake into precompiled native code as an immediate.
JL_DLLEXPORT int jl_enum_const_is_stable(jl_value_t *v) JL_NOTSAFEPOINT
{
    jl_value_t *bt = jl_typeof(v);
    if (!jl_is_datatype(bt))
        return 1;
    return enum_bits_stable((jl_datatype_t*)bt, (const char*)jl_data_ptr(v));
}

// --- package image support --------------------------------------------------

void jl_enum_write_raw_bits(void *p, uint64_t bits, size_t nbytes) JL_NOTSAFEPOINT
{
    switch (nbytes) {
    case 1: { uint8_t b = (uint8_t)bits;   memcpy(p, &b, 1); break; }
    case 2: { uint16_t b = (uint16_t)bits; memcpy(p, &b, 2); break; }
    case 4: { uint32_t b = (uint32_t)bits; memcpy(p, &b, 4); break; }
    default: memcpy(p, &bits, 8); break;
    }
}

// The member holding the same bits as *p in et's table, if that member is
// auto-assigned (i.e. its value must be rebased when serialized into a
// package image): returns its canonical instance, else NULL.
jl_value_t *jl_enum_lookup_auto_member(jl_datatype_t *et, const void *p) JL_NOTSAFEPOINT
{
    jl_svec_t *tab = jl_atomic_load_acquire(&et->name->enumtab);
    if (tab == NULL)
        return NULL;
    size_t nbytes = jl_datatype_size(enum_tab_storagetype(tab));
    size_t k = enum_find_value(tab, enum_read_raw_bits(p, nbytes), nbytes);
    if (k == (size_t)-1 || jl_svecref(tab, k + 3) == jl_true)
        return NULL;
    return jl_svecref(tab, k + 2);
}

// Filtered copy of jl_enum_member_log for serialization into a package image:
// only members of enum types that live in another image (the sysimage or a
// previously loaded package) need re-registration and value rebasing at load
// time. Enum types created by the package being precompiled serialize
// together with their member table and keep their values, which are
// deterministic (assigned in declaration order starting from an empty table).
jl_array_t *jl_enum_collect_save_list(void)
{
    jl_array_t *list = jl_alloc_vec_any(0);
    if (jl_enum_member_log == NULL)
        return list;
    JL_GC_PUSH1(&list);
    size_t n = jl_array_nrows(jl_enum_member_log);
    for (size_t i = 0; i < n; i += 5) {
        jl_value_t *et = jl_array_ptr_ref(jl_enum_member_log, i);
        if (!jl_object_in_image(et))
            continue;
        for (size_t j = 0; j < 5; j++)
            jl_array_ptr_1d_push(list, jl_array_ptr_ref(jl_enum_member_log, i + j));
    }
    JL_GC_POP();
    return list;
}

// Re-register the members in `list` (a flat stride-5 array as produced by
// jl_enum_collect_save_list, fully relocated at load time) into the live
// member tables. Members already present (same enum type, module and name)
// adopt the existing value; new explicit members keep their value; new auto
// members keep their serialized value if it is still free and are assigned a
// fresh one otherwise (this is the rebasing that deconflicts unrelated
// packages). Each member's session value is stored into `newvals` and also
// written into its serialized instance box. Explicit value conflicts throw
// before anything is mutated.
void jl_enum_restore_members(jl_array_t *list, uint64_t *newvals)
{
    size_t n = jl_array_nrows(list) / 5;
    JL_LOCK(&world_counter_lock);
    // validate explicit values first so a conflict leaves the tables untouched
    for (size_t i = 0; i < n; i++) {
        jl_datatype_t *et = (jl_datatype_t*)jl_array_ptr_ref(list, 5 * i);
        jl_module_t *mod = (jl_module_t*)jl_array_ptr_ref(list, 5 * i + 1);
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(list, 5 * i + 2);
        jl_value_t *inst = jl_array_ptr_ref(list, 5 * i + 3);
        if (jl_array_ptr_ref(list, 5 * i + 4) != jl_true)
            continue;
        jl_svec_t *tab = jl_atomic_load_relaxed(&et->name->enumtab);
        if (enum_find_member(tab, mod, name) != (size_t)-1)
            continue;
        size_t nbytes = jl_datatype_size(enum_tab_storagetype(tab));
        uint64_t bits = enum_read_bits(inst, nbytes);
        size_t taken = enum_find_value(tab, bits, nbytes);
        if (taken != (size_t)-1) {
            jl_module_t *omod = (jl_module_t*)jl_svecref(tab, taken + 1);
            jl_sym_t *oname = (jl_sym_t*)jl_svecref(tab, taken);
            jl_errorf("explicit value %llu for enum member %s.%s of %s is already taken by %s.%s",
                      (unsigned long long)bits, jl_symbol_name(mod->name),
                      jl_symbol_name(name), jl_symbol_name(et->name->name),
                      jl_symbol_name(omod->name), jl_symbol_name(oname));
        }
    }
    for (size_t i = 0; i < n; i++) {
        jl_datatype_t *et = (jl_datatype_t*)jl_array_ptr_ref(list, 5 * i);
        jl_module_t *mod = (jl_module_t*)jl_array_ptr_ref(list, 5 * i + 1);
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(list, 5 * i + 2);
        jl_value_t *inst = jl_array_ptr_ref(list, 5 * i + 3);
        int isexplicit = jl_array_ptr_ref(list, 5 * i + 4) == jl_true;
        jl_svec_t *tab = jl_atomic_load_relaxed(&et->name->enumtab);
        size_t nbytes = jl_datatype_size(enum_tab_storagetype(tab));
        size_t k = enum_find_member(tab, mod, name);
        if (k != (size_t)-1) {
            newvals[i] = enum_read_bits(jl_svecref(tab, k + 2), nbytes);
            continue;
        }
        uint64_t bits = enum_read_bits(inst, nbytes);
        if (!isexplicit && enum_find_value(tab, bits, nbytes) != (size_t)-1)
            bits = enum_next_free(tab, et, nbytes);
        // fix the serialized box immediately so table lookups (and any use
        // before the byte-patching pass) see the session value
        jl_enum_write_raw_bits(jl_data_ptr(inst), bits, nbytes);
        JL_GC_PUSH2(&tab, &inst);
        enum_tab_append(et, tab, mod, name, inst, isexplicit,
                        isexplicit ? 0 : bits + 1);
        JL_GC_POP();
        enum_log_member(et, mod, name, inst, isexplicit);
        newvals[i] = bits;
    }
    JL_UNLOCK(&world_counter_lock);
}

#ifdef __cplusplus
}
#endif
