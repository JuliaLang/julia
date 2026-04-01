// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-heap-snapshot.h"

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#include "support/strhash.h"
#include "support/ptrhash.h"

#include <stdio.h>
#include <string.h>

// https://stackoverflow.com/a/33799784/751061
static void print_str_escape_json(ios_t *stream, const char *s, size_t len) JL_NOTSAFEPOINT
{
    ios_putc('"', stream);
    for (size_t i = 0; i < len; i++) {
        char c = s[i];
        switch (c) {
        case '"':  ios_write(stream, "\\\"", 2); break;
        case '\\': ios_write(stream, "\\\\", 2); break;
        case '\b': ios_write(stream, "\\b",  2); break;
        case '\f': ios_write(stream, "\\f",  2); break;
        case '\n': ios_write(stream, "\\n",  2); break;
        case '\r': ios_write(stream, "\\r",  2); break;
        case '\t': ios_write(stream, "\\t",  2); break;
        default:
            if (('\x00' <= c) & (c <= '\x1f')) {
                ios_printf(stream, "\\u%04x", (int)(unsigned char)c);
            }
            else {
                ios_putc(c, stream);
            }
        }
    }
    ios_putc('"', stream);
}


// Edges
// "edge_fields":
//   [ "type", "name_or_index", "to_node" ]
// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2598-L2601

typedef struct {
    uint8_t type; // These *must* match the Enums on the JS side; control interpretation of name_or_index.
    size_t name_or_index; // name of the field (for objects/modules) or index of array
    size_t from_node;  // This is a deviation from the .heapsnapshot format to support streaming.
    size_t to_node;
} Edge;

// Nodes
// "node_fields":
//   [ "type", "name", "id", "self_size", "edge_count", "trace_node_id", "detachedness" ]
// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2568-L2575

typedef struct {
    uint8_t type; // index into snapshot->node_types
    size_t name;
    size_t id; // This should be a globally-unique counter, but we use the memory address
    size_t self_size;
    size_t trace_node_id;  // This is ALWAYS 0 in Javascript heap-snapshots.
    // whether the from_node is attached or detached from the main application state
    // https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/include/v8-profiler.h#L739-L745
    uint8_t detachedness;  // 0 - unknown, 1 - attached, 2 - detached
} Node;

// String table: maps string contents to sequential IDs.
// Stores internalized copies of the strings for later printing.
// When used with serialization, strings are also written to a binary stream.
typedef struct {
    htable_t map;          // strhash: string -> (void*)(size_t)id
    const char **strings;  // array of internalized string pointers (which are owned by `map`)
    size_t *lengths;       // parallel array of string lengths
    size_t count;
    size_t cap;
    size_t next_id;        // next ID to assign
} string_table_t;

static void st_init(string_table_t *t) JL_NOTSAFEPOINT
{
    memset(t, 0, sizeof(string_table_t));
    strhash_new(&t->map, 64);
}

static void st_destroy(string_table_t *t) JL_NOTSAFEPOINT
{
    strhash_free(&t->map);
    free(t->strings);
    free(t->lengths);
}

static size_t st_find_or_create(string_table_t *t, const char *key) JL_NOTSAFEPOINT
{
    // key must be NUL-terminated (strhash requires it)
    void **bp = strhash_bp(&t->map, (void *)key);

    if (*bp != HT_NOTFOUND)
        return (size_t)*bp - (size_t)HT_NOTFOUND - 1;

    size_t id = t->next_id++;
    *bp = (void *)((size_t)HT_NOTFOUND + id + 1);

    size_t len = strlen(key);

    // Store a reference to the internalized key (owned by strhash)
    if (t->count >= t->cap) {
        t->cap = t->cap ? t->cap * 2 : 64;
        t->strings = (const char **)realloc_s(t->strings, t->cap * sizeof(const char *));
        t->lengths = (size_t *)realloc_s(t->lengths, t->cap * sizeof(size_t));
    }
    // The key stored in the htable is the internalized copy (from strhash_bp's strdup)
    // We can find it by looking one slot back from bp
    t->strings[t->count] = (const char *)*(bp - 1);
    t->lengths[t->count] = len;
    t->count++;

    return id;
}

static void st_print_json_array(string_table_t *t, ios_t *stream, int newlines) JL_NOTSAFEPOINT
{
    ios_printf(stream, "[");
    for (size_t i = 0; i < t->count; i++) {
        if (i > 0)
            ios_printf(stream, newlines ? ",\n" : ",");
        print_str_escape_json(stream, t->strings[i], t->lengths[i]);
    }
    ios_printf(stream, "]");
}

// Serialize the string only if it's not already in the table
static size_t st_find_or_serialize(string_table_t *t, ios_t *stream, const char *key) JL_NOTSAFEPOINT
{
    size_t len = strlen(key);
    size_t old_count = t->count;
    size_t id = st_find_or_create(t, key);
    if (t->count > old_count) {
        // New entry — persist the string
        ios_write(stream, (const char *)&len, sizeof(size_t));
        ios_write(stream, key, len);
    }
    return id;
}

// Serialize the string unconditionally and return its index
static size_t st_serialize(string_table_t *t, ios_t *stream, const char *key) JL_NOTSAFEPOINT
{
    size_t len = strlen(key);
    ios_write(stream, (const char *)&len, sizeof(size_t));
    ios_write(stream, key, len);
    return t->next_id++;
}

typedef struct {
    // names could be very large, so we keep them in a separate binary file
    // and use a string table to keep track of the indices of frequently used strings
    // to reduce duplicates in the output file to some degree
    string_table_t names;
    // node types and edge types are very small and keep them in memory
    string_table_t node_types;
    string_table_t edge_types;
    htable_t node_ptr_to_index_map; // ptrhash: void* -> (void*)(size_t)index

    size_t num_nodes; // Since we stream out to files,
    size_t num_edges; // we need to track the counts here.

    // Used for streaming
    // Since nodes and edges are just one giant array of integers, we stream them as
    // *BINARY DATA*: a sequence of bytes, each of which is a 64-bit integer (big enough to
    // fit the pointer ids).
    ios_t *nodes;
    ios_t *edges;
    // strings are serialized to a file in binary format
    ios_t *strings;
    // the following file is written out as json data.
    ios_t *json;

    size_t internal_root_idx; // node index of the internal root node
    size_t _gc_root_idx; // node index of the GC roots node
    size_t _gc_finlist_root_idx; // node index of the GC finlist roots node
} HeapSnapshot;

// global heap snapshot, mutated by garbage collector
// when snapshotting is on.
int gc_heap_snapshot_enabled = 0;
int gc_heap_snapshot_redact_data = 0;
static HeapSnapshot *g_snapshot = NULL;
// mutex for gc-heap-snapshot.
jl_mutex_t heapsnapshot_lock;

static void final_serialize_heap_snapshot(ios_t *json, ios_t *strings, HeapSnapshot *snapshot, char all_one);
static inline void _record_gc_edge(const char *edge_type,
                                   jl_value_t *a, jl_value_t *b, size_t name_or_index) JL_NOTSAFEPOINT;
static void _record_gc_just_edge(const char *edge_type, size_t from_idx, size_t to_idx, size_t name_or_idx) JL_NOTSAFEPOINT;
static void _add_synthetic_root_entries(HeapSnapshot *snapshot) JL_NOTSAFEPOINT;


JL_DLLEXPORT void jl_gc_take_heap_snapshot(ios_t *nodes, ios_t *edges,
    ios_t *strings, ios_t *json, char all_one, char redact_data)
{
    HeapSnapshot snapshot;
    memset(&snapshot, 0, sizeof(snapshot));
    st_init(&snapshot.node_types);
    st_init(&snapshot.edge_types);
    st_init(&snapshot.names);
    htable_new(&snapshot.node_ptr_to_index_map, 1024);
    snapshot.nodes = nodes;
    snapshot.edges = edges;
    snapshot.strings = strings;
    snapshot.json = json;
    snapshot._gc_root_idx = 1;
    snapshot._gc_finlist_root_idx = 2;

    jl_mutex_lock(&heapsnapshot_lock);

    // Enable snapshotting
    g_snapshot = &snapshot;
    gc_heap_snapshot_redact_data = redact_data;
    gc_heap_snapshot_enabled = 1;

    _add_synthetic_root_entries(&snapshot);

    // Do a full GC mark (and incremental sweep), which will invoke our callbacks on `g_snapshot`
    jl_gc_collect(JL_GC_FULL);

    // Disable snapshotting
    gc_heap_snapshot_enabled = 0;
    gc_heap_snapshot_redact_data = 0;
    g_snapshot = NULL;

    jl_mutex_unlock(&heapsnapshot_lock);

    // When we return, the snapshot is full
    // Dump the snapshot
    final_serialize_heap_snapshot(json, strings, &snapshot, all_one);

    // Cleanup
    htable_free(&snapshot.node_ptr_to_index_map);
    st_destroy(&snapshot.node_types);
    st_destroy(&snapshot.edge_types);
    st_destroy(&snapshot.names);
}

static void serialize_node(HeapSnapshot *snapshot, Node node) JL_NOTSAFEPOINT
{
    // ["type","name","id","self_size","edge_count","trace_node_id","detachedness"]
    ios_write(snapshot->nodes, (char*)&node.type, sizeof(node.type));
    ios_write(snapshot->nodes, (char*)&node.name, sizeof(node.name));
    ios_write(snapshot->nodes, (char*)&node.id, sizeof(node.id));
    ios_write(snapshot->nodes, (char*)&node.self_size, sizeof(node.self_size));
    // NOTE: We don't write edge_count, since it's always 0. It will be reconstructed in
    // post-processing.
    ios_write(snapshot->nodes, (char*)&node.trace_node_id, sizeof(node.trace_node_id));
    ios_write(snapshot->nodes, (char*)&node.detachedness, sizeof(node.detachedness));

    g_snapshot->num_nodes += 1;
}

static void serialize_edge(HeapSnapshot *snapshot, Edge edge) JL_NOTSAFEPOINT
{
    // ["type","name_or_index","to_node"]
    ios_write(snapshot->edges, (char*)&edge.type, sizeof(edge.type));
    ios_write(snapshot->edges, (char*)&edge.name_or_index, sizeof(edge.name_or_index));
    // NOTE: Row numbers for nodes (not adjusted for k_node_number_of_fields, which is 7)
    ios_write(snapshot->edges, (char*)&edge.from_node, sizeof(edge.from_node));
    ios_write(snapshot->edges, (char*)&edge.to_node, sizeof(edge.to_node));

    g_snapshot->num_edges += 1;
}

// Helper to insert into the pointer->index map, returning existing index if present.
// Returns 1 if newly inserted, 0 if already present. *out_index is set either way.
static int snapshot_insert_node(HeapSnapshot *snapshot, void *ptr, size_t *out_index) JL_NOTSAFEPOINT
{
    void **bp = ptrhash_bp(&snapshot->node_ptr_to_index_map, ptr);
    if (*bp != HT_NOTFOUND) {
        *out_index = (size_t)*bp - (size_t)HT_NOTFOUND - 1;
        return 0; // already present
    }
    size_t idx = snapshot->num_nodes;
    *bp = (void *)((size_t)HT_NOTFOUND + idx + 1);
    *out_index = idx;
    return 1; // newly inserted
}

// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L212
// add synthetic nodes for the uber root, the GC roots, and the GC finalizer list roots
static void _add_synthetic_root_entries(HeapSnapshot *snapshot) JL_NOTSAFEPOINT
{
    // adds a node at id 0 which is the "uber root":
    // a synthetic node which points to all the GC roots.
    Node internal_root = {
        (uint8_t)st_find_or_create(&snapshot->node_types, "synthetic"),
        st_find_or_serialize(&snapshot->names, snapshot->strings, ""), // name
        0, // id
        0, // size
        0, // size_t trace_node_id (unused)
        0 // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(snapshot, internal_root);

    // Add a node for the GC roots
    snapshot->_gc_root_idx = snapshot->internal_root_idx + 1;
    Node gc_roots = {
        (uint8_t)st_find_or_create(&snapshot->node_types, "synthetic"),
        st_find_or_serialize(&snapshot->names, snapshot->strings, "GC roots"), // name
        snapshot->_gc_root_idx, // id
        0, // size
        0, // size_t trace_node_id (unused)
        0 // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(snapshot, gc_roots);
    Edge root_to_gc_roots = {
        (uint8_t)st_find_or_create(&snapshot->edge_types, "internal"),
        st_find_or_serialize(&snapshot->names, snapshot->strings, "GC roots"), // edge label
        snapshot->internal_root_idx, // from
        snapshot->_gc_root_idx // to
    };
    serialize_edge(snapshot, root_to_gc_roots);

    // add a node for the gc finalizer list roots
    snapshot->_gc_finlist_root_idx = snapshot->internal_root_idx + 2;
    Node gc_finlist_roots = {
        (uint8_t)st_find_or_create(&snapshot->node_types, "synthetic"),
        st_find_or_serialize(&snapshot->names, snapshot->strings, "GC finalizer list roots"), // name
        snapshot->_gc_finlist_root_idx, // id
        0, // size
        0, // size_t trace_node_id (unused)
        0 // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(snapshot, gc_finlist_roots);
    Edge root_to_gc_finlist_roots = {
        (uint8_t)st_find_or_create(&snapshot->edge_types, "internal"),
        st_find_or_serialize(&snapshot->names, snapshot->strings, "GC finalizer list roots"), // edge label
        snapshot->internal_root_idx, // from
        snapshot->_gc_finlist_root_idx // to
    };
    serialize_edge(snapshot, root_to_gc_finlist_roots);
}

// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L597-L597
// returns the index of the new node
size_t record_node_to_gc_snapshot(jl_value_t *a) JL_NOTSAFEPOINT
{
    size_t idx;
    if (!snapshot_insert_node(g_snapshot, a, &idx))
        return idx;

    ios_t str_;
    int ios_need_close = 0;

    // Insert a new Node
    size_t self_size = 0;
    const char *name = "<missing>";
    const char *node_type = "object";

    jl_datatype_t *type = (jl_datatype_t*)jl_typeof(a);

    if (jl_is_string(a)) {
        node_type = "String";
        name = gc_heap_snapshot_redact_data ? "<redacted>" : jl_string_data(a);
        self_size = jl_string_len(a);
    }
    else if (jl_is_symbol(a)) {
        node_type = "jl_sym_t";
        name = jl_symbol_name((jl_sym_t*)a);
        self_size = strlen(name);
    }
    else if (jl_is_simplevector(a)) {
        node_type = "jl_svec_t";
        name = "SimpleVector";
        self_size = sizeof(jl_svec_t) + sizeof(void*) * jl_svec_len(a);
    }
    else if (jl_is_module(a)) {
        node_type = "jl_module_t";
        name = jl_symbol_name_(((jl_module_t*)a)->name);
        self_size = sizeof(jl_module_t);
    }
    else if (jl_is_task(a)) {
        node_type = "jl_task_t";
        name = "Task";
        self_size = sizeof(jl_task_t);
    }
    else if (jl_is_datatype(a)) {
        ios_need_close = 1;
        ios_mem(&str_, 0);
        JL_STREAM* str = (JL_STREAM*)&str_;
        jl_static_show(str, a);
        ios_putc('\0', &str_); // NUL-terminate
        name = (const char*)str_.buf;
        node_type = "jl_datatype_t";
        self_size = sizeof(jl_datatype_t);
    }
    else if (jl_is_array(a)){
        ios_need_close = 1;
        ios_mem(&str_, 0);
        JL_STREAM* str = (JL_STREAM*)&str_;
        jl_static_show(str, (jl_value_t*)type);
        ios_putc('\0', &str_); // NUL-terminate
        name = (const char*)str_.buf;
        node_type = "jl_array_t";
        self_size = sizeof(jl_array_t);
    }
    else {
        self_size = (size_t)jl_datatype_size(type);
        if (type->name == jl_genericmemory_typename) {
            jl_genericmemory_t *mem = (jl_genericmemory_t*)a;
            int how = jl_genericmemory_how(mem);
            if (how != JL_GENERICMEMORY_STRINGOWNED && how != JL_GENERICMEMORY_MALLOCD) {
                // Memory's that are string-owned or point to foreign memory have
                // explicit snapshot edges to pointee data. Otherwise the array
                // contents are treated as part of the Memory itself.
                self_size += jl_genericmemory_nbytes(mem);
            }
        }
        // print full type into ios buffer and get StringRef to it.
        // The ios is cleaned up below.
        ios_need_close = 1;
        ios_mem(&str_, 0);
        JL_STREAM* str = (JL_STREAM*)&str_;
        jl_static_show(str, (jl_value_t*)type);
        ios_putc('\0', &str_); // NUL-terminate
        node_type = (const char*)str_.buf;
        name = (const char*)str_.buf;
    }

    Node node = {
        (uint8_t)st_find_or_create(&g_snapshot->node_types, node_type), // size_t type;
        st_serialize(&g_snapshot->names, g_snapshot->strings, name), // size_t name;
        (size_t)a,     // size_t id;
        // We add 1 to self-size for the type tag that all heap-allocated objects have.
        // Also because the Chrome Snapshot viewer ignores size-0 leaves!
        sizeof(void*) + self_size, // size_t self_size;
        0,             // size_t trace_node_id (unused)
        0,             // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(g_snapshot, node);

    if (ios_need_close)
        ios_close(&str_);

    return idx;
}

static size_t record_pointer_to_gc_snapshot(void *a, size_t bytes, const char *name) JL_NOTSAFEPOINT
{
    size_t idx;
    if (!snapshot_insert_node(g_snapshot, a, &idx))
        return idx;

    Node node = {
        (uint8_t)st_find_or_create(&g_snapshot->node_types, "object"), // size_t type;
        st_serialize(&g_snapshot->names, g_snapshot->strings, name), // size_t name;
        (size_t)a,     // size_t id;
        bytes,         // size_t self_size;
        0,             // size_t trace_node_id (unused)
        0,             // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(g_snapshot, node);

    return idx;
}

static void _fieldpath_for_slot(void *obj, void *slot, ios_t *result) JL_NOTSAFEPOINT
{
    jl_datatype_t *objtype = (jl_datatype_t*)jl_typeof(obj);

    while (1) {
        int i = gc_slot_to_fieldidx(obj, slot, objtype);

        if (jl_is_tuple_type(objtype) || jl_is_namedtuple_type(objtype)) {
            ios_printf(result, "[%d]", i);
        }
        else {
            jl_svec_t *field_names = jl_field_names(objtype);
            jl_sym_t *name = (jl_sym_t*)jl_svecref(field_names, i);
            ios_puts(jl_symbol_name(name), result);
        }

        if (!jl_field_isptr(objtype, i)) {
            // Tail recurse
            ios_putc('.', result);
            obj = (void*)((char*)obj + jl_field_offset(objtype, i));
            objtype = (jl_datatype_t*)jl_field_type_concrete(objtype, i);
        }
        else {
            return;
        }
    }
}

void _gc_heap_snapshot_record_root(jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    size_t to_node_idx = record_node_to_gc_snapshot(root);
    size_t edge_label = st_serialize(&g_snapshot->names, g_snapshot->strings, name);

    _record_gc_just_edge("internal", g_snapshot->internal_root_idx, to_node_idx, edge_label);
}

void _gc_heap_snapshot_record_gc_roots(jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    size_t to_node_idx = record_node_to_gc_snapshot(root);
    size_t edge_label = st_serialize(&g_snapshot->names, g_snapshot->strings, name);

    _record_gc_just_edge("internal", g_snapshot->_gc_root_idx, to_node_idx, edge_label);
}

void _gc_heap_snapshot_record_finlist(jl_value_t *obj, size_t index) JL_NOTSAFEPOINT
{
    size_t to_node_idx = record_node_to_gc_snapshot(obj);
    char ss[32];
    snprintf(ss, sizeof(ss), "finlist-%zu", index);
    size_t edge_label = st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, ss);
    _record_gc_just_edge("internal", g_snapshot->_gc_finlist_root_idx, to_node_idx, edge_label);
}

// Add a node to the heap snapshot representing a Julia stack frame.
// Each task points at a stack frame, which points at the stack frame of
// the function it's currently calling, forming a linked list.
// Stack frame nodes point at the objects they have as local variables.
size_t _record_stack_frame_node(HeapSnapshot *snapshot, void *frame) JL_NOTSAFEPOINT
{
    size_t idx;
    if (!snapshot_insert_node(g_snapshot, frame, &idx))
        return idx;

    Node node = {
        (uint8_t)st_find_or_create(&snapshot->node_types, "synthetic"),
        st_find_or_serialize(&snapshot->names, snapshot->strings, "(stack frame)"), // name
        (size_t)frame, // id
        1, // size
        0, // size_t trace_node_id (unused)
        0, // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(snapshot, node);

    return idx;
}

void _gc_heap_snapshot_record_frame_to_object_edge(void *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    size_t from_node_idx = _record_stack_frame_node(g_snapshot, (jl_gcframe_t*)from);
    size_t to_idx = record_node_to_gc_snapshot(to);

    size_t name_idx = st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, "local var");
    _record_gc_just_edge("internal", from_node_idx, to_idx, name_idx);
}

void _gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, void *to) JL_NOTSAFEPOINT
{
    size_t from_node_idx = record_node_to_gc_snapshot((jl_value_t*)from);
    size_t to_node_idx = _record_stack_frame_node(g_snapshot, to);

    size_t name_idx = st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, "stack");
    _record_gc_just_edge("internal", from_node_idx, to_node_idx, name_idx);
}

void _gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT
{
    size_t from_node_idx = _record_stack_frame_node(g_snapshot, from);
    size_t to_node_idx = _record_stack_frame_node(g_snapshot, to);

    size_t name_idx = st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, "next frame");
    _record_gc_just_edge("internal", from_node_idx, to_node_idx, name_idx);
}

void _gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT
{
    _record_gc_edge("element", from, to, index);
}

void _gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, void *slot) JL_NOTSAFEPOINT
{
    ios_t path;
    ios_mem(&path, 128);
    _fieldpath_for_slot(from, slot, &path);
    ios_putc('\0', &path); // NUL-terminate for st_find_or_serialize
    size_t name_idx = st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, (const char *)path.buf);
    ios_close(&path);
    _record_gc_edge("property", from, to, name_idx);
}

void _gc_heap_snapshot_record_module_to_binding(jl_module_t *module, jl_value_t *bindings, jl_value_t *bindingkeyset) JL_NOTSAFEPOINT
{
    size_t from_node_idx = record_node_to_gc_snapshot((jl_value_t*)module);
    size_t to_bindings_idx = record_node_to_gc_snapshot(bindings);
    size_t to_bindingkeyset_idx = record_node_to_gc_snapshot(bindingkeyset);

    if (to_bindings_idx > 0) {
        _record_gc_just_edge("internal", from_node_idx, to_bindings_idx, st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, "bindings"));
    }
    if (to_bindingkeyset_idx > 0) {
        _record_gc_just_edge("internal", from_node_idx, to_bindingkeyset_idx, st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, "bindingkeyset"));
    }
 }

void _gc_heap_snapshot_record_internal_array_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    _record_gc_edge("internal", from, to,
                    st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, "<internal>"));
}

void _gc_heap_snapshot_record_binding_partition_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    _record_gc_edge("binding", from, to,
                    st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, "<binding>"));
}


void _gc_heap_snapshot_record_foreign_memory_edge(jl_value_t *from, void* to, size_t bytes) JL_NOTSAFEPOINT
{
    size_t name_or_idx = st_find_or_serialize(&g_snapshot->names, g_snapshot->strings, "<native>");

    size_t from_node_idx = record_node_to_gc_snapshot(from);
    const char *alloc_kind = "<foreign memory - malloc>";
    size_t to_node_idx = record_pointer_to_gc_snapshot(to, bytes, alloc_kind);

    _record_gc_just_edge("hidden", from_node_idx, to_node_idx, name_or_idx);
}

static inline void _record_gc_edge(const char *edge_type, jl_value_t *a,
                                  jl_value_t *b, size_t name_or_idx) JL_NOTSAFEPOINT
{
    size_t from_node_idx = record_node_to_gc_snapshot(a);
    size_t to_node_idx = record_node_to_gc_snapshot(b);

    _record_gc_just_edge(edge_type, from_node_idx, to_node_idx, name_or_idx);
}

static void _record_gc_just_edge(const char *edge_type, size_t from_idx, size_t to_idx, size_t name_or_idx) JL_NOTSAFEPOINT
{
    Edge edge = {
        (uint8_t)st_find_or_create(&g_snapshot->edge_types, edge_type),
        name_or_idx, // edge label
        from_idx, // from
        to_idx // to
    };

    serialize_edge(g_snapshot, edge);
}

static void final_serialize_heap_snapshot(ios_t *json, ios_t *strings, HeapSnapshot *snapshot, char all_one)
{
    // mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2567-L2567
    // also https://github.com/microsoft/vscode-v8-heap-tools/blob/c5b34396392397925ecbb4ecb904a27a2754f2c1/v8-heap-parser/src/decoder.rs#L43-L51
    ios_printf(json, "{\"snapshot\":{\n");

    ios_printf(json, "  \"meta\":{\n");
    ios_printf(json, "    \"node_fields\":[\"type\",\"name\",\"id\",\"self_size\",\"edge_count\",\"trace_node_id\",\"detachedness\"],\n");
    ios_printf(json, "    \"node_types\":[");
    st_print_json_array(&snapshot->node_types, json, 0);
    ios_printf(json, ",");
    ios_printf(json, "\"string\", \"number\", \"number\", \"number\", \"number\", \"number\"],\n");
    ios_printf(json, "    \"edge_fields\":[\"type\",\"name_or_index\",\"to_node\"],\n");
    ios_printf(json, "    \"edge_types\":[");
    st_print_json_array(&snapshot->edge_types, json, 0);
    ios_printf(json, ",");
    ios_printf(json, "\"string_or_number\",\"from_node\"],\n");
    // not used. Required by microsoft/vscode-v8-heap-tools
    ios_printf(json, "    \"trace_function_info_fields\":[\"function_id\",\"name\",\"script_name\",\"script_id\",\"line\",\"column\"],\n");
    ios_printf(json, "    \"trace_node_fields\":[\"id\",\"function_info_index\",\"count\",\"size\",\"children\"],\n");
    ios_printf(json, "    \"sample_fields\":[\"timestamp_us\",\"last_assigned_id\"],\n");
    ios_printf(json, "    \"location_fields\":[\"object_index\",\"script_id\",\"line\",\"column\"]\n");
    // end not used
    ios_printf(json, "  },\n"); // end "meta"

    ios_printf(json, "  \"node_count\":%zu,\n", snapshot->num_nodes);
    ios_printf(json, "  \"edge_count\":%zu,\n", snapshot->num_edges);
    ios_printf(json, "  \"trace_function_count\":0\n"); // not used. Required by microsoft/vscode-v8-heap-tools
    ios_printf(json, "}\n"); // end "snapshot"

    // this } is removed by the julia reassembler in Profile
    ios_printf(json, "}");
}
