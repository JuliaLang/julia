// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-heap-snapshot.h"

#include "julia_internal.h"
#include "julia_assert.h"
#include "gc.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/DenseMap.h"

#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <set>

using std::string;
using std::set;
using std::ostringstream;
using std::pair;
using std::make_pair;
using llvm::SmallVector;
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
    uint8_t type; // These *must* match the Enums on the JS side; control interpretation of name_or_index.
    size_t name_or_index; // name of the field (for objects/modules) or index of array
    size_t from_node;  // This is a deviation from the .heapsnapshot format to support streaming.
    size_t to_node;
};

// Nodes
// "node_fields":
//   [ "type", "name", "id", "self_size", "edge_count", "trace_node_id", "detachedness" ]
// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2568-L2575

struct Node {
    uint8_t type; // index into snapshot->node_types
    size_t name;
    size_t id; // This should be a globally-unique counter, but we use the memory address
    size_t self_size;
    size_t trace_node_id;  // This is ALWAYS 0 in Javascript heap-snapshots.
    // whether the from_node is attached or detached from the main application state
    // https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/include/v8-profiler.h#L739-L745
    uint8_t detachedness;  // 0 - unknown, 1 - attached, 2 - detached

    ~Node() JL_NOTSAFEPOINT = default;
};

class StringTable {
protected:
    StringMap<size_t> map;
    SmallVector<StringRef, 0> strings;
    size_t next_id;

public:
    StringTable() JL_NOTSAFEPOINT : map(), strings(), next_id(0) {};

    size_t find_or_create_string_id(StringRef key) JL_NOTSAFEPOINT {
        auto val = map.insert(make_pair(key, next_id));
        if (val.second) {
            strings.push_back(val.first->first());
            next_id++;
        }
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

// a string table with partial strings in memory and all strings serialized to a file
class SerializedStringTable: public StringTable {
    public:

    // serialize the string only if it's not already in the table
    size_t serialize_if_necessary(ios_t *stream, StringRef key) JL_NOTSAFEPOINT {
        auto val = map.insert(make_pair(key, next_id));
        if (val.second) {
            strings.push_back(val.first->first());
            // persist the string size first, then the string itself
            // so that we could read it back in the same order
            size_t s_size = key.size();
            ios_write(stream, reinterpret_cast<const char*>(&s_size), sizeof(size_t));
            ios_write(stream, key.data(), s_size);
            next_id++;
        }
        return val.first->second;
    }

    // serialize the string without checking if it is in the table or not
    // and return its index. This means that we might have duplicates in the
    // output string file.
    size_t serialize(ios_t *stream, StringRef key) JL_NOTSAFEPOINT {
        size_t s_size = key.size();
        ios_write(stream, reinterpret_cast<const char*>(&s_size), sizeof(size_t));
        ios_write(stream, key.data(), s_size);
        size_t current = next_id;
        next_id++;
        return current;
    }
};

struct HeapSnapshot {
    // names could be very large, so we keep them in a separate binary file
    // and use a StringTable to keep track of the indices of frequently used strings
    // to reduce duplicates in the output file to some degree
    SerializedStringTable names;
    // node types and edge types are very small and keep them in memory
    StringTable node_types;
    StringTable edge_types;
    DenseMap<void *, size_t> node_ptr_to_index_map;

    size_t num_nodes = 0; // Since we stream out to files,
    size_t num_edges = 0; // we need to track the counts here.

    // Node internal_root;

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

    size_t internal_root_idx = 0; // node index of the internal root node
    size_t _gc_root_idx = 1; // node index of the GC roots node
    size_t _gc_finlist_root_idx = 2; // node index of the GC finlist roots node
};

// global heap snapshot, mutated by garbage collector
// when snapshotting is on.
int gc_heap_snapshot_enabled = 0;
HeapSnapshot *g_snapshot = nullptr;
extern jl_mutex_t heapsnapshot_lock;

void final_serialize_heap_snapshot(ios_t *json, ios_t *strings, HeapSnapshot &snapshot, char all_one);
void serialize_heap_snapshot(ios_t *stream, HeapSnapshot &snapshot, char all_one);
static inline void _record_gc_edge(const char *edge_type,
                                   jl_value_t *a, jl_value_t *b, size_t name_or_index) JL_NOTSAFEPOINT;
void _record_gc_just_edge(const char *edge_type, size_t from_idx, size_t to_idx, size_t name_or_idx) JL_NOTSAFEPOINT;
void _add_synthetic_root_entries(HeapSnapshot *snapshot) JL_NOTSAFEPOINT;


JL_DLLEXPORT void jl_gc_take_heap_snapshot(ios_t *nodes, ios_t *edges,
    ios_t *strings, ios_t *json, char all_one)
{
    HeapSnapshot snapshot;
    snapshot.nodes = nodes;
    snapshot.edges = edges;
    snapshot.strings = strings;
    snapshot.json = json;

    jl_mutex_lock(&heapsnapshot_lock);

    // Enable snapshotting
    g_snapshot = &snapshot;
    gc_heap_snapshot_enabled = true;

    _add_synthetic_root_entries(&snapshot);

    // Do a full GC mark (and incremental sweep), which will invoke our callbacks on `g_snapshot`
    jl_gc_collect(JL_GC_FULL);

    // Disable snapshotting
    gc_heap_snapshot_enabled = false;
    g_snapshot = nullptr;

    jl_mutex_unlock(&heapsnapshot_lock);

    // When we return, the snapshot is full
    // Dump the snapshot
    final_serialize_heap_snapshot((ios_t*)json, (ios_t*)strings, snapshot, all_one);
}

void serialize_node(HeapSnapshot *snapshot, const Node &node) JL_NOTSAFEPOINT
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

void serialize_edge(HeapSnapshot *snapshot, const Edge &edge) JL_NOTSAFEPOINT
{
    // ["type","name_or_index","to_node"]
    ios_write(snapshot->edges, (char*)&edge.type, sizeof(edge.type));
    ios_write(snapshot->edges, (char*)&edge.name_or_index, sizeof(edge.name_or_index));
    // NOTE: Row numbers for nodes (not adjusted for k_node_number_of_fields, which is 7)
    ios_write(snapshot->edges, (char*)&edge.from_node, sizeof(edge.from_node));
    ios_write(snapshot->edges, (char*)&edge.to_node, sizeof(edge.to_node));

    g_snapshot->num_edges += 1;
}

// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L212
// add synthetic nodes for the uber root, the GC roots, and the GC finalizer list roots
void _add_synthetic_root_entries(HeapSnapshot *snapshot) JL_NOTSAFEPOINT
{
    // adds a node at id 0 which is the "uber root":
    // a synthetic node which points to all the GC roots.
    Node internal_root{
        (uint8_t)snapshot->node_types.find_or_create_string_id("synthetic"),
        snapshot->names.serialize_if_necessary(snapshot->strings, ""), // name
        0, // id
        0, // size
        0, // size_t trace_node_id (unused)
        0 // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(snapshot, internal_root);

    // Add a node for the GC roots
    snapshot->_gc_root_idx = snapshot->internal_root_idx + 1;
    Node gc_roots{
        (uint8_t)snapshot->node_types.find_or_create_string_id("synthetic"),
        snapshot->names.serialize_if_necessary(snapshot->strings, "GC roots"), // name
        snapshot->_gc_root_idx, // id
        0, // size
        0, // size_t trace_node_id (unused)
        0 // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(snapshot, gc_roots);
    Edge root_to_gc_roots{
        (uint8_t)snapshot->edge_types.find_or_create_string_id("internal"),
        snapshot->names.serialize_if_necessary(snapshot->strings, "GC roots"), // edge label
        snapshot->internal_root_idx, // from
        snapshot->_gc_root_idx // to
    };
    serialize_edge(snapshot, root_to_gc_roots);

    // add a node for the gc finalizer list roots
    snapshot->_gc_finlist_root_idx = snapshot->internal_root_idx + 2;
    Node gc_finlist_roots{
        (uint8_t)snapshot->node_types.find_or_create_string_id("synthetic"),
        snapshot->names.serialize_if_necessary(snapshot->strings, "GC finalizer list roots"), // name
        snapshot->_gc_finlist_root_idx, // id
        0, // size
        0, // size_t trace_node_id (unused)
        0 // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(snapshot, gc_finlist_roots);
    Edge root_to_gc_finlist_roots{
        (uint8_t)snapshot->edge_types.find_or_create_string_id("internal"),
        snapshot->names.serialize_if_necessary(snapshot->strings, "GC finalizer list roots"), // edge label
        snapshot->internal_root_idx, // from
        snapshot->_gc_finlist_root_idx // to
    };
    serialize_edge(snapshot, root_to_gc_finlist_roots);
}

// mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L597-L597
// returns the index of the new node
size_t record_node_to_gc_snapshot(jl_value_t *a) JL_NOTSAFEPOINT
{
    auto val = g_snapshot->node_ptr_to_index_map.insert(make_pair(a, g_snapshot->num_nodes));
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

    auto node = Node{
        (uint8_t)g_snapshot->node_types.find_or_create_string_id(node_type), // size_t type;
        g_snapshot->names.serialize(g_snapshot->strings, name), // size_t name;
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

    return val.first->second;
}

static size_t record_pointer_to_gc_snapshot(void *a, size_t bytes, StringRef name) JL_NOTSAFEPOINT
{
    auto val = g_snapshot->node_ptr_to_index_map.insert(make_pair(a, g_snapshot->num_nodes));
    if (!val.second) {
        return val.first->second;
    }

    auto node = Node{
        (uint8_t)g_snapshot->node_types.find_or_create_string_id( "object"), // size_t type;
        g_snapshot->names.serialize(g_snapshot->strings, name), // size_t name;
        (size_t)a,     // size_t id;
        bytes,         // size_t self_size;
        0,             // size_t trace_node_id (unused)
        0,             // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(g_snapshot, node);

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
    size_t to_node_idx = record_node_to_gc_snapshot(root);
    auto edge_label = g_snapshot->names.serialize(g_snapshot->strings, name);

    _record_gc_just_edge("internal", g_snapshot->internal_root_idx, to_node_idx, edge_label);
}

void _gc_heap_snapshot_record_gc_roots(jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    auto to_node_idx = record_node_to_gc_snapshot(root);
    auto edge_label = g_snapshot->names.serialize(g_snapshot->strings, name);

    _record_gc_just_edge("internal", g_snapshot->_gc_root_idx, to_node_idx, edge_label);
}

void _gc_heap_snapshot_record_finlist(jl_value_t *obj, size_t index) JL_NOTSAFEPOINT
{
    auto to_node_idx = record_node_to_gc_snapshot(obj);
    ostringstream ss;
    ss << "finlist-" << index;
    auto edge_label = g_snapshot->names.serialize_if_necessary(g_snapshot->strings, ss.str());
    _record_gc_just_edge("internal", g_snapshot->_gc_finlist_root_idx, to_node_idx, edge_label);
}

// Add a node to the heap snapshot representing a Julia stack frame.
// Each task points at a stack frame, which points at the stack frame of
// the function it's currently calling, forming a linked list.
// Stack frame nodes point at the objects they have as local variables.
size_t _record_stack_frame_node(HeapSnapshot *snapshot, void *frame) JL_NOTSAFEPOINT
{
    auto val = g_snapshot->node_ptr_to_index_map.insert(make_pair(frame, g_snapshot->num_nodes));
    if (!val.second) {
        return val.first->second;
    }

    auto node = Node{
        (uint8_t)snapshot->node_types.find_or_create_string_id("synthetic"),
        snapshot->names.serialize_if_necessary(snapshot->strings, "(stack frame)"), // name
        (size_t)frame, // id
        1, // size
        0, // size_t trace_node_id (unused)
        0, // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    serialize_node(snapshot, node);

    return val.first->second;
}

void _gc_heap_snapshot_record_frame_to_object_edge(void *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    auto from_node_idx = _record_stack_frame_node(g_snapshot, (jl_gcframe_t*)from);
    auto to_idx = record_node_to_gc_snapshot(to);

    auto name_idx = g_snapshot->names.serialize_if_necessary(g_snapshot->strings, "local var");
    _record_gc_just_edge("internal", from_node_idx, to_idx, name_idx);
}

void _gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, void *to) JL_NOTSAFEPOINT
{
    auto from_node_idx = record_node_to_gc_snapshot((jl_value_t*)from);
    auto to_node_idx = _record_stack_frame_node(g_snapshot, to);

    auto name_idx = g_snapshot->names.serialize_if_necessary(g_snapshot->strings, "stack");
    _record_gc_just_edge("internal", from_node_idx, to_node_idx, name_idx);
}

void _gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT
{
    auto from_node_idx = _record_stack_frame_node(g_snapshot, from);
    auto to_node_idx = _record_stack_frame_node(g_snapshot, to);

    auto name_idx = g_snapshot->names.serialize_if_necessary(g_snapshot->strings, "next frame");
    _record_gc_just_edge("internal", from_node_idx, to_node_idx, name_idx);
}

void _gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT
{
    _record_gc_edge("element", from, to, index);
}

void _gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, void *slot) JL_NOTSAFEPOINT
{
    string path = _fieldpath_for_slot(from, slot);
    _record_gc_edge("property", from, to,
                    g_snapshot->names.serialize_if_necessary(g_snapshot->strings, path));
}

void _gc_heap_snapshot_record_module_to_binding(jl_module_t *module, jl_value_t *bindings, jl_value_t *bindingkeyset) JL_NOTSAFEPOINT
{
    auto from_node_idx = record_node_to_gc_snapshot((jl_value_t*)module);
    auto to_bindings_idx = record_node_to_gc_snapshot(bindings);
    auto to_bindingkeyset_idx = record_node_to_gc_snapshot(bindingkeyset);

    if (to_bindings_idx > 0) {
        _record_gc_just_edge("internal", from_node_idx, to_bindings_idx, g_snapshot->names.serialize_if_necessary(g_snapshot->strings, "bindings"));
    }
    if (to_bindingkeyset_idx > 0) {
        _record_gc_just_edge("internal", from_node_idx, to_bindingkeyset_idx, g_snapshot->names.serialize_if_necessary(g_snapshot->strings, "bindingkeyset"));
    }
 }

void _gc_heap_snapshot_record_internal_array_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    _record_gc_edge("internal", from, to,
                    g_snapshot->names.serialize_if_necessary(g_snapshot->strings, "<internal>"));
}

void _gc_heap_snapshot_record_hidden_edge(jl_value_t *from, void* to, size_t bytes, uint16_t alloc_type) JL_NOTSAFEPOINT
{
    // valid alloc_type values are 0, 1, 2
    assert(alloc_type <= 2);
    size_t name_or_idx = g_snapshot->names.serialize_if_necessary(g_snapshot->strings, "<native>");

    auto from_node_idx = record_node_to_gc_snapshot(from);
    const char *alloc_kind = NULL;
    switch (alloc_type)
    {
    case 0:
        alloc_kind = "<generic memory - malloc>";
        break;
    case 1:
        alloc_kind = "<generic memory - pool alloc>";
        break;
    case 2:
        alloc_kind = "<generic memory - inline alloc>";
        break;
    }
    auto to_node_idx = record_pointer_to_gc_snapshot(to, bytes, alloc_kind);

    _record_gc_just_edge("hidden", from_node_idx, to_node_idx, name_or_idx);
}

static inline void _record_gc_edge(const char *edge_type, jl_value_t *a,
                                  jl_value_t *b, size_t name_or_idx) JL_NOTSAFEPOINT
{
    auto from_node_idx = record_node_to_gc_snapshot(a);
    auto to_node_idx = record_node_to_gc_snapshot(b);

    _record_gc_just_edge(edge_type, from_node_idx, to_node_idx, name_or_idx);
}

void _record_gc_just_edge(const char *edge_type, size_t from_idx, size_t to_idx, size_t name_or_idx) JL_NOTSAFEPOINT
{
    auto edge = Edge{
        (uint8_t)g_snapshot->edge_types.find_or_create_string_id(edge_type),
        name_or_idx, // edge label
        from_idx, // from
        to_idx // to
    };

    serialize_edge(g_snapshot, edge);
}

void final_serialize_heap_snapshot(ios_t *json, ios_t *strings, HeapSnapshot &snapshot, char all_one)
{
    // mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2567-L2567
    // also https://github.com/microsoft/vscode-v8-heap-tools/blob/c5b34396392397925ecbb4ecb904a27a2754f2c1/v8-heap-parser/src/decoder.rs#L43-L51
    ios_printf(json, "{\"snapshot\":{\n");

    ios_printf(json, "  \"meta\":{\n");
    ios_printf(json, "    \"node_fields\":[\"type\",\"name\",\"id\",\"self_size\",\"edge_count\",\"trace_node_id\",\"detachedness\"],\n");
    ios_printf(json, "    \"node_types\":[");
    snapshot.node_types.print_json_array(json, false);
    ios_printf(json, ",");
    ios_printf(json, "\"string\", \"number\", \"number\", \"number\", \"number\", \"number\"],\n");
    ios_printf(json, "    \"edge_fields\":[\"type\",\"name_or_index\",\"to_node\"],\n");
    ios_printf(json, "    \"edge_types\":[");
    snapshot.edge_types.print_json_array(json, false);
    ios_printf(json, ",");
    ios_printf(json, "\"string_or_number\",\"from_node\"],\n");
    // not used. Required by microsoft/vscode-v8-heap-tools
    ios_printf(json, "    \"trace_function_info_fields\":[\"function_id\",\"name\",\"script_name\",\"script_id\",\"line\",\"column\"],\n");
    ios_printf(json, "    \"trace_node_fields\":[\"id\",\"function_info_index\",\"count\",\"size\",\"children\"],\n");
    ios_printf(json, "    \"sample_fields\":[\"timestamp_us\",\"last_assigned_id\"],\n");
    ios_printf(json, "    \"location_fields\":[\"object_index\",\"script_id\",\"line\",\"column\"]\n");
    // end not used
    ios_printf(json, "  },\n"); // end "meta"

    ios_printf(json, "  \"node_count\":%zu,\n", snapshot.num_nodes);
    ios_printf(json, "  \"edge_count\":%zu,\n", snapshot.num_edges);
    ios_printf(json, "  \"trace_function_count\":0\n"); // not used. Required by microsoft/vscode-v8-heap-tools
    ios_printf(json, "}\n"); // end "snapshot"

    // this } is removed by the julia reassembler in Profile
    ios_printf(json, "}");
}
