// This file is a part of Julia. License is MIT: https://julialang.org/license

/* Version 3 is a graph archive, not a restricted solver serialization. Include
 * after subtype-corpus.inc's ordinary encoder. The caller holds scorpus_lock.
 * No operation here allocates Julia objects or reaches a Julia safepoint.
 */
typedef struct {
    htable_t ids;
    arraylist_t objects;
} scorpus_full_graph;

static htable_t scorpus_full_identity_ids;
static arraylist_t scorpus_full_identity_roots;
static arraylist_t scorpus_full_identity_flags;
static arraylist_t scorpus_full_stable_typenames;
static int scorpus_full_identity_initialized;

static void scorpus_full_roots(int full) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = jl_current_task->ptls;
    for (size_t i = 0; i < scorpus_full_identity_roots.len; i++)
        jl_gc_mark_queue_obj(ptls, (jl_value_t*)scorpus_full_identity_roots.items[i]);
}

// Process-scoped IDs are counters, never addresses or objectid hashes. Retaining
// these objects prevents address reuse from aliasing an earlier identity.
static size_t scorpus_full_identity(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (!scorpus_full_identity_initialized) {
        htable_new(&scorpus_full_identity_ids, 0);
        arraylist_new(&scorpus_full_identity_roots, 0);
        arraylist_new(&scorpus_full_identity_flags, 0);
        arraylist_new(&scorpus_full_stable_typenames, 0);
        jl_gc_set_cb_root_scanner(scorpus_full_roots, 1);
        scorpus_full_identity_initialized = 1;
    }
    void **slot = ptrhash_bp(&scorpus_full_identity_ids, v);
    if (*slot != HT_NOTFOUND)
        return (size_t)(uintptr_t)*slot - 1;
    size_t id = scorpus_full_identity_roots.len + 1;
    arraylist_push(&scorpus_full_identity_roots, v);
    arraylist_push(&scorpus_full_identity_flags, NULL);
    *slot = (void*)(uintptr_t)(id + 1);
    return id;
}

static size_t scorpus_full_node(scorpus_full_graph *g, jl_value_t *v) JL_NOTSAFEPOINT
{
    if (!v)
        return 0;
    void **slot = ptrhash_bp(&g->ids, v);
    if (*slot != HT_NOTFOUND)
        return (size_t)(uintptr_t)*slot - 1;
    size_t id = g->objects.len + 1;
    arraylist_push(&g->objects, v);
    *slot = (void*)(uintptr_t)(id + 1);
    return id;
}

static void scorpus_full_edge(scorpus_full_graph *g, ios_t *o, const char *key,
                              jl_value_t *v) JL_NOTSAFEPOINT
{
    ios_printf(o, "%s = %zu\n", key, scorpus_full_node(g, v));
}

static void scorpus_full_hex(ios_t *o, const void *data, size_t len) JL_NOTSAFEPOINT
{
    static const char hex[] = "0123456789abcdef";
    const unsigned char *s = (const unsigned char*)data;
    ios_putc('"', o);
    for (size_t i = 0; i < len; i++) {
        ios_putc(hex[s[i] >> 4], o);
        ios_putc(hex[s[i] & 15], o);
    }
    ios_putc('"', o);
}

static void scorpus_full_process_id(ios_t *o, jl_value_t *v) JL_NOTSAFEPOINT
{
    ios_printf(o, "identity = \"process-%d-%016" PRIx64 "-%zu\"\n",
               (int)uv_os_getpid(), scorpus_session, scorpus_full_identity(v));
}

static void scorpus_full_vector(scorpus_full_graph *g, ios_t *o, const char *key,
                                jl_svec_t *items) JL_NOTSAFEPOINT
{
    ios_printf(o, "%s = [", key);
    if (items) {
        for (size_t i = 0; i < jl_svec_len(items); i++)
            ios_printf(o, "%s%zu", i ? "," : "", scorpus_full_node(g, jl_svecref(items, i)));
    }
    ios_puts("]\n", o);
}

static void scorpus_full_symbol(ios_t *o, const char *key, jl_sym_t *sym) JL_NOTSAFEPOINT
{
    ios_printf(o, "%s = ", key);
    const char *s = sym ? jl_symbol_name(sym) : "";
    // Symbols can contain non-UTF8 bytes. Hex keeps every TOML record valid.
    scorpus_full_hex(o, s, strlen(s));
    ios_putc('\n', o);
}

static int scorpus_full_canonical_typename(jl_typename_t *tn) JL_NOTSAFEPOINT
{
    if (!tn->module || !tn->name || !tn->wrapper ||
        (tn->module != jl_core_module && !jl_astaggedvalue(tn)->bits.in_image))
        return 0;
    jl_svec_t *bindings = jl_atomic_load_acquire(&tn->module->bindings);
    if (!bindings)
        return 0;
    for (size_t i = 0; i < jl_svec_len(bindings); i++) {
        jl_value_t *v = jl_svecref(bindings, i);
        if (!v || v == jl_nothing)
            continue;
        jl_binding_t *b = (jl_binding_t*)v;
        if (b->globalref->name == tn->name)
            return jl_get_latest_binding_value_if_resolved_and_const_debug_only(b) == tn->wrapper;
    }
    return 0;
}

static int scorpus_full_typename_stable(jl_typename_t *tn) JL_NOTSAFEPOINT
{
    size_t id = scorpus_full_identity((jl_value_t*)tn);
    uintptr_t flag = (uintptr_t)scorpus_full_identity_flags.items[id - 1];
    if (flag)
        return flag == 1;
    int stable = scorpus_full_canonical_typename(tn);
    // Freeze the choice. Rebinding a canonical family must neither change the
    // old family's identity nor let a replacement reuse its stable name key.
    for (size_t i = 0; stable && i < scorpus_full_stable_typenames.len; i++) {
        jl_typename_t *old = (jl_typename_t*)scorpus_full_stable_typenames.items[i];
        if (old->module == tn->module && old->name == tn->name)
            stable = 0;
    }
    scorpus_full_identity_flags.items[id - 1] = (void*)(uintptr_t)(stable ? 1 : 2);
    if (stable)
        arraylist_push(&scorpus_full_stable_typenames, tn);
    return stable;
}

static int scorpus_full_contains_native_pointer(jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    scorpus_full_graph types;
    htable_new(&types.ids, 0);
    arraylist_new(&types.objects, 0);
    scorpus_full_node(&types, (jl_value_t*)dt);
    int found = 0;
    for (size_t i = 0; i < types.objects.len && !found; i++) {
        jl_value_t *v = (jl_value_t*)types.objects.items[i];
        if (jl_is_uniontype(v)) {
            scorpus_full_node(&types, ((jl_uniontype_t*)v)->a);
            scorpus_full_node(&types, ((jl_uniontype_t*)v)->b);
        }
        else if (jl_is_datatype(v)) {
            jl_datatype_t *fieldtype = (jl_datatype_t*)v;
            if (fieldtype->name && (jl_is_cpointer_type(v) ||
                                   fieldtype->name == jl_llvmpointer_typename)) {
                found = 1;
                break;
            }
            if (fieldtype->types && fieldtype->layout &&
                !jl_is_layout_opaque(fieldtype->layout) &&
                fieldtype->layout->flags.fielddesc_type != JL_FIELDDESC_FOREIGN) {
                for (size_t j = 0; j < jl_svec_len(fieldtype->types); j++)
                    if (j < fieldtype->layout->nfields && !jl_field_isptr(fieldtype, j))
                        scorpus_full_node(&types, jl_svecref(fieldtype->types, j));
            }
        }
    }
    htable_free(&types.ids);
    arraylist_free(&types.objects);
    return found;
}

static void scorpus_full_value(scorpus_full_graph *g, ios_t *o, jl_value_t *v) JL_NOTSAFEPOINT
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(v);
    const jl_datatype_layout_t *layout = dt->layout;
    if (scorpus_full_contains_native_pointer(dt)) {
        ios_puts("kind = \"Opaque\"\n", o);
        scorpus_full_edge(g, o, "type", (jl_value_t*)dt);
        scorpus_full_process_id(o, v);
        ios_puts("reason = \"native-pointer-value\"\n", o);
        return;
    }
    // Mutable values and foreign/opaque layouts do not have a coherent immutable
    // snapshot. Preserve their identity explicitly rather than omit the input.
    if (!dt->name || dt->name->mutabl || !layout || layout->flags.haspadding ||
        jl_is_layout_opaque(layout) || layout->flags.fielddesc_type == JL_FIELDDESC_FOREIGN) {
        ios_puts("kind = \"Opaque\"\n", o);
        scorpus_full_edge(g, o, "type", (jl_value_t*)dt);
        scorpus_full_process_id(o, v);
        ios_puts("reason = \"mutable-padded-or-special-layout\"\n", o);
        return;
    }
    ios_puts("kind = \"Value\"\n", o);
    scorpus_full_edge(g, o, "type", (jl_value_t*)dt);
    if (dt->isprimitivetype) {
        ios_puts("bytes = ", o);
        scorpus_full_hex(o, v, layout->size);
        ios_printf(o, "\nbits = %u\n", jl_datatype_nbits(dt));
        return;
    }
    // Record pointer slots separately, including pointers inside inlined fields.
    // The raw payload has those slots zeroed, so GC addresses never enter keys.
    unsigned char *payload = (unsigned char*)calloc_s(layout->size ? layout->size : 1);
    // Top-level padding is omitted; inline field storage is retained verbatim
    // except for pointer slots, and is explicitly a native-layout snapshot.
    for (size_t i = 0; i < layout->nfields; i++) {
        uint32_t offset = jl_field_offset(dt, i), size = jl_field_size(dt, i);
        if (offset <= layout->size && size <= layout->size - offset)
            memcpy(payload + offset, (char*)v + offset, size);
    }
    ios_puts("pointer_offsets = [", o);
    for (size_t i = 0; i < layout->npointers; i++) {
        size_t offset = (size_t)jl_ptr_offset(dt, i) * sizeof(void*);
        ios_printf(o, "%s%zu", i ? "," : "", offset);
        memset(payload + offset, 0, sizeof(void*));
    }
    ios_puts("]\npointer_refs = [", o);
    for (size_t i = 0; i < layout->npointers; i++) {
        size_t offset = (size_t)jl_ptr_offset(dt, i) * sizeof(void*);
        jl_value_t *child;
        memcpy(&child, (char*)v + offset, sizeof(child));
        ios_printf(o, "%s%zu", i ? "," : "", scorpus_full_node(g, child));
    }
    ios_puts("]\nbytes = ", o);
    scorpus_full_hex(o, payload, layout->size);
    ios_puts("\nencoding = \"native-layout-pointer-masked\"\n", o);
    free(payload);
}

static void scorpus_full_emit(scorpus_full_graph *g, ios_t *o, jl_value_t *v) JL_NOTSAFEPOINT
{
#define FULL_KIND(s) ios_puts("kind = \"" s "\"\n", o)
#define FULL_EDGE(k, v) scorpus_full_edge(g, o, k, (jl_value_t*)(v))
    if (v == (jl_value_t*)jl_any_type)
        FULL_KIND("Any");
    else if (v == jl_bottom_type)
        FULL_KIND("Bottom");
    else if (jl_is_uniontype(v)) {
        FULL_KIND("Union");
        FULL_EDGE("a", ((jl_uniontype_t*)v)->a);
        FULL_EDGE("b", ((jl_uniontype_t*)v)->b);
    }
    else if (jl_is_intersecttype(v)) {
        FULL_KIND("Intersect");
        FULL_EDGE("a", ((jl_intersecttype_t*)v)->a);
        FULL_EDGE("b", ((jl_intersecttype_t*)v)->b);
    }
    else if (jl_is_nottype(v)) {
        FULL_KIND("Not");
        FULL_EDGE("child", ((jl_nottype_t*)v)->body);
    }
    else if (jl_is_typevar(v)) {
        jl_tvar_t *tv = (jl_tvar_t*)v;
        FULL_KIND("TypeVar");
        scorpus_full_symbol(o, "name_hex", tv->name);
        FULL_EDGE("lb", tv->lb);
        FULL_EDGE("ub", tv->ub);
    }
    else if (jl_is_unionall(v)) {
        FULL_KIND("UnionAll");
        FULL_EDGE("var", ((jl_unionall_t*)v)->var);
        FULL_EDGE("body", ((jl_unionall_t*)v)->body);
    }
    else if (jl_is_vararg(v)) {
        FULL_KIND("Vararg");
        FULL_EDGE("T", ((jl_vararg_t*)v)->T);
        FULL_EDGE("N", ((jl_vararg_t*)v)->N);
    }
    else if (jl_is_typeeq(v) || jl_is_typeegal(v)) {
        if (jl_is_typeeq(v)) FULL_KIND("TypeEq"); else FULL_KIND("TypeEgal");
        FULL_EDGE("T", ((jl_typeeq_t*)v)->T);
    }
    else if (jl_is_typeapp(v)) {
        FULL_KIND("TypeApp");
        FULL_EDGE("head", ((jl_typeapp_t*)v)->head);
        FULL_EDGE("param", ((jl_typeapp_t*)v)->param);
    }
    else if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        FULL_KIND("DataType");
        FULL_EDGE("typename", dt->name);
        FULL_EDGE("super", dt->super);
        scorpus_full_vector(g, o, "params", dt->parameters);
        ios_printf(o, "abstract = %s\nprimitive = %s\nconcrete = %s\n",
                   dt->name && dt->name->abstract ? "true" : "false", dt->isprimitivetype ? "true" : "false",
                   dt->isconcretetype ? "true" : "false");
        ios_printf(o, "typename_available = %s\n", dt->name ? "true" : "false");
        if (dt->isprimitivetype && dt->layout)
            ios_printf(o, "bits = %u\n", jl_datatype_nbits(dt));

    }
    else if (jl_is_typename(v)) {
        jl_typename_t *tn = (jl_typename_t*)v;
        FULL_KIND("TypeName");
        scorpus_full_symbol(o, "name_hex", tn->name);
        FULL_EDGE("module", tn->module);
        FULL_EDGE("wrapper", tn->wrapper);
        FULL_EDGE("field_names", tn->names);
        ios_printf(o, "abstract = %s\nmutable = %s\n", tn->abstract ? "true" : "false", tn->mutabl ? "true" : "false");
        if (scorpus_full_typename_stable(tn))
            ios_puts("identity = \"canonical-module-name\"\n", o);
        else
            scorpus_full_process_id(o, v);
    }
    else if (jl_is_symbol(v)) {
        FULL_KIND("Symbol");
        scorpus_full_symbol(o, "bytes", (jl_sym_t*)v);
    }
    else if (jl_is_string(v)) {
        FULL_KIND("String");
        ios_puts("bytes = ", o);
        scorpus_full_hex(o, jl_string_data(v), jl_string_len(v));
        ios_putc('\n', o);
    }
    else if (jl_is_svec(v)) {
        FULL_KIND("SimpleVector");
        scorpus_full_vector(g, o, "items", (jl_svec_t*)v);
    }
    else if (jl_is_module(v)) {
        jl_module_t *m = (jl_module_t*)v;
        FULL_KIND("Module");
        scorpus_full_symbol(o, "name_hex", m->name);
        FULL_EDGE("parent", m->parent);
        ios_printf(o, "uuid = \"%016" PRIx64 "%016" PRIx64 "\"\nbuild_id = \"%016" PRIx64 "%016" PRIx64 "\"\n",
                   m->uuid.hi, m->uuid.lo, m->build_id.hi, m->build_id.lo);
        if (m == jl_core_module || m == jl_main_module || jl_astaggedvalue(m)->bits.in_image)
            ios_puts("identity = \"image-module\"\n", o);
        else
            scorpus_full_process_id(o, v);
    }
    else
        scorpus_full_value(g, o, v);
#undef FULL_EDGE
#undef FULL_KIND
}

static char *scorpus_encode_full(jl_value_t *x, jl_value_t *y, size_t *len) JL_NOTSAFEPOINT
{
    scorpus_full_graph g;
    htable_new(&g.ids, 0);
    arraylist_new(&g.objects, 0);
    ios_t o;
    ios_mem(&o, 1024);
    size_t lhs = scorpus_full_node(&g, x), rhs = scorpus_full_node(&g, y);
    const uint16_t byteorder = 1;
    ios_printf(&o, "version = 3\nkind = \"subtype\"\nlhs = %zu\nrhs = %zu\npointer_bytes = %zu\nbyte_order = \"%s\"\n",
               lhs, rhs, sizeof(void*), *(const uint8_t*)&byteorder ? "little" : "big");
    // Emission appends unseen objects to this worklist. No recursion, grammar
    // limits, or fixed node capacity; object interning terminates graph cycles.
    for (size_t i = 0; i < g.objects.len; i++) {
        jl_value_t *v = (jl_value_t*)g.objects.items[i];
        ios_printf(&o, "\n[[nodes]]\nid = %zu\n", i + 1);
        scorpus_full_emit(&g, &o, v);
    }
    htable_free(&g.ids);
    arraylist_free(&g.objects);
    char *key = ios_take_buffer(&o, len);
    ios_close(&o);
    (*len)--;
    return key;
}
