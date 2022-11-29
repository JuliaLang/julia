// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-heap-snapshot.h"

#include "julia_internal.h"
#include "gc.h"

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/DenseMap.h"

#include <vector>
#include <string>
#include <sstream>

using std::vector;
using std::string;
using std::ostringstream;
using std::pair;
using std::make_pair;
using llvm::StringMap;
using llvm::DenseMap;
using llvm::StringRef;

// https://stackoverflow.com/a/33799784/751061
void print_str_escape_json(ios_t *stream, StringRef s)
{
    ios_putc('"', stream);
    for (auto c = s.begin(); c != s.end(); c++) {
        switch (*c) {
        case '"':  ios_write(stream, "\\\"", 2); break;
        case '\\': ios_write(stream, "\\\\", 2); break;
        case '\b': ios_write(stream, "\\b",  2); break;
        case '\f': ios_write(stream, "\\f",  2); break;
        case '\n': ios_write(stream, "\\n",  2); break;
        case '\r': ios_write(stream, "\\r",  2); break;
        case '\t': ios_write(stream, "\\t",  2); break;
        default:
            if (('\x00' <= *c) & (*c <= '\x1f')) {
                ios_printf(stream, "\\u%04x", (int)*c);
            }
            else {
                ios_putc(*c, stream);
            }
        }
    }
    ios_putc('"', stream);
}


// Edges
// "edge_fields":
//   [ "type", "name_or_index", "to_node" ]
// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2598-L2601

struct Edge {
    size_t type; // These *must* match the Enums on the JS side; control interpretation of name_or_index.
    size_t name_or_index; // name of the field (for objects/modules) or index of array
    size_t to_node;
};

// Nodes
// "node_fields":
//   [ "type", "name", "id", "self_size", "edge_count", "trace_node_id", "detachedness" ]
// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2568-L2575

const int k_node_number_of_fields = 7;
struct Node {
    size_t type; // index into snapshot->node_types
    size_t name;
    size_t id; // This should be a globally-unique counter, but we use the memory address
    size_t self_size;
    size_t trace_node_id;  // This is ALWAYS 0 in Javascript heap-snapshots.
    // whether the from_node is attached or dettached from the main application state
    // https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/include/v8-profiler.h#L739-L745
    int detachedness;  // 0 - unknown, 1 - attached, 2 - detached
    vector<Edge> edges;

    ~Node() JL_NOTSAFEPOINT = default;
};

struct StringTable {
    StringMap<size_t> map;
    vector<StringRef> strings;

    size_t find_or_create_string_id(StringRef key) JL_NOTSAFEPOINT {
        auto val = map.insert(make_pair(key, map.size()));
        if (val.second)
            strings.push_back(val.first->first());
        return val.first->second;
    }

    void print_json_array(ios_t *stream, bool newlines) {
        ios_printf(stream, "[");
        bool first = true;
        for (const auto &str : strings) {
            if (first) {
                first = false;
            }
            else {
                ios_printf(stream, newlines ? ",\n" : ",");
            }
            print_str_escape_json(stream, str);
        }
        ios_printf(stream, "]");
    }
};

struct HeapSnapshot {
    vector<Node> nodes;
    // edges are stored on each from_node

    StringTable names;
    StringTable node_types;
    StringTable edge_types;
    DenseMap<void *, size_t> node_ptr_to_index_map;

    size_t num_edges = 0; // For metadata, updated as you add each edge. Needed because edges owned by nodes.
};

// global heap snapshot, mutated by garbage collector
// when snapshotting is on.
int gc_heap_snapshot_enabled = 0;
HeapSnapshot *g_snapshot = nullptr;
extern jl_mutex_t heapsnapshot_lock;

void serialize_heap_snapshot(ios_t *stream, HeapSnapshot &snapshot, char all_one);
static inline void _record_gc_edge(const char *edge_type,
                                   jl_value_t *a, jl_value_t *b, size_t name_or_index) JL_NOTSAFEPOINT;
void _record_gc_just_edge(const char *edge_type, Node &from_node, size_t to_idx, size_t name_or_idx) JL_NOTSAFEPOINT;
void _add_internal_root(HeapSnapshot *snapshot);


JL_DLLEXPORT void jl_gc_take_heap_snapshot(ios_t *stream, char all_one)
{
    HeapSnapshot snapshot;
    _add_internal_root(&snapshot);

    jl_mutex_lock(&heapsnapshot_lock);

    // Enable snapshotting
    g_snapshot = &snapshot;
    gc_heap_snapshot_enabled = true;

    // Do a full GC mark (and incremental sweep), which will invoke our callbacks on `g_snapshot`
    jl_gc_collect(JL_GC_FULL);

    // Disable snapshotting
    gc_heap_snapshot_enabled = false;
    g_snapshot = nullptr;

    jl_mutex_unlock(&heapsnapshot_lock);

    // When we return, the snapshot is full
    // Dump the snapshot
    serialize_heap_snapshot((ios_t*)stream, snapshot, all_one);
}

// adds a node at id 0 which is the "uber root":
// a synthetic node which points to all the GC roots.
void _add_internal_root(HeapSnapshot *snapshot)
{
    Node internal_root{
        snapshot->node_types.find_or_create_string_id("synthetic"),
        snapshot->names.find_or_create_string_id(""), // name
        0, // id
        0, // size
        0, // size_t trace_node_id (unused)
        0, // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
        vector<Edge>() // outgoing edges
    };
    snapshot->nodes.push_back(internal_root);
}

// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L597-L597
// returns the index of the new node
size_t record_node_to_gc_snapshot(jl_value_t *a) JL_NOTSAFEPOINT
{
    auto val = g_snapshot->node_ptr_to_index_map.insert(make_pair(a, g_snapshot->nodes.size()));
    if (!val.second) {
        return val.first->second;
    }

    ios_t str_;
    bool ios_need_close = 0;

    // Insert a new Node
    size_t self_size = 0;
    StringRef name = "<missing>";
    StringRef node_type = "object";

    jl_datatype_t *type = (jl_datatype_t*)jl_typeof(a);

    if (jl_is_string(a)) {
        node_type = "String";
        name = jl_string_data(a);
        self_size = jl_string_len(a);
    }
    else if (jl_is_symbol(a)) {
        node_type = "jl_sym_t";
        name = jl_symbol_name((jl_sym_t*)a);
        self_size = name.size();
    }
    else if (jl_is_simplevector(a)) {
        node_type = "jl_svec_t";
        name = "SimpleVector";
        self_size = sizeof(jl_svec_t) + sizeof(void*) * jl_svec_len(a);
    }
    else if (jl_is_module(a)) {
        node_type = "jl_module_t";
        name = jl_symbol_name_(((_jl_module_t*)a)->name);
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
        name = StringRef((const char*)str_.buf, str_.size);
        node_type = "jl_datatype_t";
        self_size = sizeof(jl_datatype_t);
    }
    else if (jl_is_array(a)){
        ios_need_close = 1;
        ios_mem(&str_, 0);
        JL_STREAM* str = (JL_STREAM*)&str_;
        jl_static_show(str, (jl_value_t*)type);
        name = StringRef((const char*)str_.buf, str_.size);
        node_type = "jl_array_t";
        self_size = sizeof(jl_array_t);
    }
    else {
        self_size = (size_t)jl_datatype_size(type);
        // print full type into ios buffer and get StringRef to it.
        // The ios is cleaned up below.
        ios_need_close = 1;
        ios_mem(&str_, 0);
        JL_STREAM* str = (JL_STREAM*)&str_;
        jl_static_show(str, (jl_value_t*)type);

        name = StringRef((const char*)str_.buf, str_.size);
    }

    g_snapshot->nodes.push_back(Node{
        g_snapshot->node_types.find_or_create_string_id(node_type), // size_t type;
        g_snapshot->names.find_or_create_string_id(name), // size_t name;
        (size_t)a,     // size_t id;
        // We add 1 to self-size for the type tag that all heap-allocated objects have.
        // Also because the Chrome Snapshot viewer ignores size-0 leaves!
        sizeof(void*) + self_size, // size_t self_size;
        0,             // size_t trace_node_id (unused)
        0,             // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
        vector<Edge>() // outgoing edges
    });

    if (ios_need_close)
        ios_close(&str_);

    return val.first->second;
}

static size_t record_pointer_to_gc_snapshot(void *a, size_t bytes, StringRef name) JL_NOTSAFEPOINT
{
    auto val = g_snapshot->node_ptr_to_index_map.insert(make_pair(a, g_snapshot->nodes.size()));
    if (!val.second) {
        return val.first->second;
    }

    g_snapshot->nodes.push_back(Node{
        g_snapshot->node_types.find_or_create_string_id( "object"), // size_t type;
        g_snapshot->names.find_or_create_string_id(name), // size_t name;
        (size_t)a,     // size_t id;
        bytes,         // size_t self_size;
        0,             // size_t trace_node_id (unused)
        0,             // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
        vector<Edge>() // outgoing edges
    });

    return val.first->second;
}

static string _fieldpath_for_slot(void *obj, void *slot) JL_NOTSAFEPOINT
{
    string res;
    jl_datatype_t *objtype = (jl_datatype_t*)jl_typeof(obj);

    while (1) {
        int i = gc_slot_to_fieldidx(obj, slot, objtype);

        if (jl_is_tuple_type(objtype) || jl_is_namedtuple_type(objtype)) {
            ostringstream ss;
            ss << "[" << i << "]";
            res += ss.str();
        }
        else {
            jl_svec_t *field_names = jl_field_names(objtype);
            jl_sym_t *name = (jl_sym_t*)jl_svecref(field_names, i);
            res += jl_symbol_name(name);
        }

        if (!jl_field_isptr(objtype, i)) {
            // Tail recurse
            res += ".";
            obj = (void*)((char*)obj + jl_field_offset(objtype, i));
            objtype = (jl_datatype_t*)jl_field_type_concrete(objtype, i);
        }
        else {
            return res;
        }
    }
}


void _gc_heap_snapshot_record_root(jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    record_node_to_gc_snapshot(root);

    auto &internal_root = g_snapshot->nodes.front();
    auto to_node_idx = g_snapshot->node_ptr_to_index_map[root];
    auto edge_label = g_snapshot->names.find_or_create_string_id(name);

    _record_gc_just_edge("internal", internal_root, to_node_idx, edge_label);
}

// Add a node to the heap snapshot representing a Julia stack frame.
// Each task points at a stack frame, which points at the stack frame of
// the function it's currently calling, forming a linked list.
// Stack frame nodes point at the objects they have as local variables.
size_t _record_stack_frame_node(HeapSnapshot *snapshot, void *frame) JL_NOTSAFEPOINT
{
    auto val = g_snapshot->node_ptr_to_index_map.insert(make_pair(frame, g_snapshot->nodes.size()));
    if (!val.second) {
        return val.first->second;
    }

    snapshot->nodes.push_back(Node{
        snapshot->node_types.find_or_create_string_id("synthetic"),
        snapshot->names.find_or_create_string_id("(stack frame)"), // name
        (size_t)frame, // id
        1, // size
        0, // size_t trace_node_id (unused)
        0, // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
        vector<Edge>() // outgoing edges
    });

    return val.first->second;
}

void _gc_heap_snapshot_record_frame_to_object_edge(void *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    auto from_node_idx = _record_stack_frame_node(g_snapshot, (jl_gcframe_t*)from);
    auto to_idx = record_node_to_gc_snapshot(to);
    Node &from_node = g_snapshot->nodes[from_node_idx];

    auto name_idx = g_snapshot->names.find_or_create_string_id("local var");
    _record_gc_just_edge("internal", from_node, to_idx, name_idx);
}

void _gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, void *to) JL_NOTSAFEPOINT
{
    auto from_node_idx = record_node_to_gc_snapshot((jl_value_t*)from);
    auto to_node_idx = _record_stack_frame_node(g_snapshot, to);
    Node &from_node = g_snapshot->nodes[from_node_idx];

    auto name_idx = g_snapshot->names.find_or_create_string_id("stack");
    _record_gc_just_edge("internal", from_node, to_node_idx, name_idx);
}

void _gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT
{
    auto from_node_idx = _record_stack_frame_node(g_snapshot, from);
    auto to_node_idx = _record_stack_frame_node(g_snapshot, to);
    Node &from_node = g_snapshot->nodes[from_node_idx];

    auto name_idx = g_snapshot->names.find_or_create_string_id("next frame");
    _record_gc_just_edge("internal", from_node, to_node_idx, name_idx);
}

void _gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT
{
    _record_gc_edge("element", from, to, index);
}

void _gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, void *slot) JL_NOTSAFEPOINT
{
    string path = _fieldpath_for_slot(from, slot);
    _record_gc_edge("property", from, to,
                    g_snapshot->names.find_or_create_string_id(path));
}

void _gc_heap_snapshot_record_module_to_binding(jl_module_t* module, jl_binding_t* binding) JL_NOTSAFEPOINT
{
    auto from_node_idx = record_node_to_gc_snapshot((jl_value_t*)module);
    auto to_node_idx = record_pointer_to_gc_snapshot(binding, sizeof(jl_binding_t), jl_symbol_name(binding->name));

    jl_value_t *value = jl_atomic_load_relaxed(&binding->value);
    auto value_idx = value ? record_node_to_gc_snapshot(value) : 0;
    jl_value_t *ty = jl_atomic_load_relaxed(&binding->ty);
    auto ty_idx = ty ? record_node_to_gc_snapshot(ty) : 0;
    jl_value_t *globalref = jl_atomic_load_relaxed(&binding->globalref);
    auto globalref_idx = globalref ? record_node_to_gc_snapshot(globalref) : 0;

    auto &from_node = g_snapshot->nodes[from_node_idx];
    auto &to_node = g_snapshot->nodes[to_node_idx];

    _record_gc_just_edge("property", from_node, to_node_idx, g_snapshot->names.find_or_create_string_id("<native>"));
    if (value_idx)     _record_gc_just_edge("internal", to_node, value_idx, g_snapshot->names.find_or_create_string_id("value"));
    if (ty_idx)        _record_gc_just_edge("internal", to_node, ty_idx, g_snapshot->names.find_or_create_string_id("ty"));
    if (globalref_idx) _record_gc_just_edge("internal", to_node, globalref_idx, g_snapshot->names.find_or_create_string_id("globalref"));
}

void _gc_heap_snapshot_record_internal_array_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    _record_gc_edge("internal", from, to,
                    g_snapshot->names.find_or_create_string_id("<internal>"));
}

void _gc_heap_snapshot_record_hidden_edge(jl_value_t *from, void* to, size_t bytes, uint16_t alloc_type) JL_NOTSAFEPOINT
{
    size_t name_or_idx = g_snapshot->names.find_or_create_string_id("<native>");

    auto from_node_idx = record_node_to_gc_snapshot(from);
    const char *alloc_kind;
    switch (alloc_type)
    {
    case 0:
        alloc_kind = "<malloc>";
        break;
    case 1:
        alloc_kind = "<pooled>";
        break;
    case 2:
        alloc_kind = "<inline>";
        break;
    default:
        alloc_kind = "<undef>";
        break;
    }
    auto to_node_idx = record_pointer_to_gc_snapshot(to, bytes, alloc_kind);
    auto &from_node = g_snapshot->nodes[from_node_idx];

    _record_gc_just_edge("hidden", from_node, to_node_idx, name_or_idx);
}

static inline void _record_gc_edge(const char *edge_type, jl_value_t *a,
                                  jl_value_t *b, size_t name_or_idx) JL_NOTSAFEPOINT
{
    auto from_node_idx = record_node_to_gc_snapshot(a);
    auto to_node_idx = record_node_to_gc_snapshot(b);

    auto &from_node = g_snapshot->nodes[from_node_idx];

    _record_gc_just_edge(edge_type, from_node, to_node_idx, name_or_idx);
}

void _record_gc_just_edge(const char *edge_type, Node &from_node, size_t to_idx, size_t name_or_idx) JL_NOTSAFEPOINT
{
    from_node.edges.push_back(Edge{
        g_snapshot->edge_types.find_or_create_string_id(edge_type),
        name_or_idx, // edge label
        to_idx // to
    });

    g_snapshot->num_edges += 1;
}

void serialize_heap_snapshot(ios_t *stream, HeapSnapshot &snapshot, char all_one)
{
    // mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2567-L2567
    ios_printf(stream, "{\"snapshot\":{");
    ios_printf(stream, "\"meta\":{");
    ios_printf(stream, "\"node_fields\":[\"type\",\"name\",\"id\",\"self_size\",\"edge_count\",\"trace_node_id\",\"detachedness\"],");
    ios_printf(stream, "\"node_types\":[");
    snapshot.node_types.print_json_array(stream, false);
    ios_printf(stream, ",");
    ios_printf(stream, "\"string\", \"number\", \"number\", \"number\", \"number\", \"number\"],");
    ios_printf(stream, "\"edge_fields\":[\"type\",\"name_or_index\",\"to_node\"],");
    ios_printf(stream, "\"edge_types\":[");
    snapshot.edge_types.print_json_array(stream, false);
    ios_printf(stream, ",");
    ios_printf(stream, "\"string_or_number\",\"from_node\"]");
    ios_printf(stream, "},\n"); // end "meta"
    ios_printf(stream, "\"node_count\":%zu,", snapshot.nodes.size());
    ios_printf(stream, "\"edge_count\":%zu", snapshot.num_edges);
    ios_printf(stream, "},\n"); // end "snapshot"

    ios_printf(stream, "\"nodes\":[");
    bool first_node = true;
    for (const auto &from_node : snapshot.nodes) {
        if (first_node) {
            first_node = false;
        }
        else {
            ios_printf(stream, ",");
        }
        // ["type","name","id","self_size","edge_count","trace_node_id","detachedness"]
        ios_printf(stream, "%zu,%zu,%zu,%zu,%zu,%zu,%d\n",
                            from_node.type,
                            from_node.name,
                            from_node.id,
                            all_one ? (size_t)1 : from_node.self_size,
                            from_node.edges.size(),
                            from_node.trace_node_id,
                            from_node.detachedness);
    }
    ios_printf(stream, "],\n");

    ios_printf(stream, "\"edges\":[");
    bool first_edge = true;
    for (const auto &from_node : snapshot.nodes) {
        for (const auto &edge : from_node.edges) {
            if (first_edge) {
                first_edge = false;
            }
            else {
                ios_printf(stream, ",");
            }
            ios_printf(stream, "%zu,%zu,%zu\n",
                                edge.type,
                                edge.name_or_index,
                                edge.to_node * k_node_number_of_fields);
        }
    }
    ios_printf(stream, "],\n"); // end "edges"

    ios_printf(stream, "\"strings\":");

    snapshot.names.print_json_array(stream, true);

    ios_printf(stream, "}");
}
