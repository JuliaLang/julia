#include "gc-heap-snapshot.h"

#include "julia_internal.h"
#include "gc.h"

#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <iostream>

using std::cout; using std::endl;
using std::vector;
using std::string;
using std::unordered_map;
using std::unordered_set;

// https://stackoverflow.com/a/33799784/751061
void print_str_escape_json(JL_STREAM *stream, const std::string &s) {
    jl_printf(stream, "\"");
    for (auto c = s.cbegin(); c != s.cend(); c++) {
        switch (*c) {
        case '"': jl_printf(stream, "\\\""); break;
        case '\\': jl_printf(stream, "\\\\"); break;
        case '\b': jl_printf(stream, "\\b"); break;
        case '\f': jl_printf(stream, "\\f"); break;
        case '\n': jl_printf(stream, "\\n"); break;
        case '\r': jl_printf(stream, "\\r"); break;
        case '\t': jl_printf(stream, "\\t"); break;
        default:
            if ('\x00' <= *c && *c <= '\x1f') {
                jl_printf(stream, "\\u%04x", (int)*c);
            } else {
                jl_printf(stream, "%c", *c);
            }
        }
    }
    jl_printf(stream, "\"");
}

struct HeapSnapshot;
void serialize_heap_snapshot(JL_STREAM *stream, HeapSnapshot &snapshot);

// Dump format:
// Nodes
// "node_fields":
//   [ "type", "name", "id", "self_size", "edge_count", "trace_node_id", "detachedness" ]

const int k_node_number_of_fields = 7;
struct Node {
    string type;
    string name;
    size_t id; // (vilterp) the memory address, right?
    size_t self_size;
    size_t edge_count;
    size_t trace_node_id;  // This is ALWAYS 0 in Javascript heap-snapshots.
    // whether the node is attached or dettached from the main application state
    // TODO: .... meaning not yet understood.
    // https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/include/v8-profiler.h#L739-L745
    int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
};

// Edges
// "edge_fields":
//   [ "type", "name_or_index", "to_node" ]

const int k_edge_number_of_fields = 3;

struct Edge {
    string type; // These *must* match the Enums on the JS side; control interpretation of name_or_index.
    size_t name_or_index; // name of the field (for objects/modules) or index of array
    size_t to_node;

    // Book-keeping fields (not used for serialization)
    size_t from_node; // For asserting that we built the edges in the right order
};

//template<typename K, typename V>
//auto find_or_insert_iter(unordered_map<K,V>& map, const K &key) {
//}

struct StringTable {
    typedef unordered_map<string, size_t> MapType;

    MapType map;
    vector<string> strings;

    StringTable() {}
    StringTable(std::initializer_list<string> strs) : strings(strs) {
        for (const auto& str : strs) {
            map.insert({str, map.size()});
        }
    }

    size_t find_or_create_string_id(string key) {
        auto val = map.find(key);
        if (val == map.end()) {
            val = map.insert(val, {key, map.size()});
            strings.push_back(key);
        }
        return val->second;
    }

    void print_json_array(JL_STREAM *stream, bool newlines) {
        jl_printf(stream, "[");
        bool first = true;
        for (const auto &str : strings) {
            if (first) {
                first = false;
            } else {
                jl_printf(stream, ",");
                if (newlines) {
                    jl_printf(stream, "\n");
                }
            }
            // Escape strings for JSON
            // TODO
            print_str_escape_json(stream, str);
        }
        jl_printf(stream, "]");
    }
};

struct HeapSnapshot {
public:

// private:
    vector<Node> nodes;

    vector<Edge> edges;

    StringTable names;
    StringTable node_types = {"object"};
    StringTable edge_types = {"property"};
    unordered_map<void*, size_t> node_ptr_to_index_map;
};


// TODO: Do we need to refer to nodes by their index in the node array?
//size_t find_or_create_node_id(HeapSnapshot& snapshot, string key) {
//    return find_or_insert_iter(snapshot.nodes_map, key)->second;
//}

HeapSnapshot *g_snapshot = nullptr;

JL_DLLEXPORT int count_nodes = 0;
JL_DLLEXPORT int count_edges = 0;

JL_DLLEXPORT void jl_gc_take_heap_snapshot(JL_STREAM *stream) {
    // Enable snapshotting
    HeapSnapshot snapshot;
    g_snapshot = &snapshot;

    // Do GC, which will callback into record_edge_to_gc_snapshot()...
    jl_gc_collect(JL_GC_FULL);

    // Disable snapshotting
    g_snapshot = nullptr;

    // When we return, the snapshot is full
    // Dump the snapshot
    serialize_heap_snapshot(stream, snapshot);

    // Debugging
    //jl_printf(JL_STDERR, "nodes: %d\n", count_nodes);
    //jl_printf(JL_STDERR, "edges: %d\n", count_edges);
}

JL_DLLEXPORT void record_node_to_gc_snapshot(jl_value_t *a) {
    auto val = g_snapshot->node_ptr_to_index_map.find((void*)a);
    if (val != g_snapshot->node_ptr_to_index_map.end()) {
        return;
        //return &g_snapshot->nodes[val->second];
    }
    // Insert a new Node
    jl_datatype_t* type = (jl_datatype_t*)jl_typeof(a);

    size_t self_size = 1;
    string name = "<missing>";

    if ((uintptr_t)type < 4096U) {
        name = "<unkown>";
    } else if (type == (jl_datatype_t*)jl_buff_tag) {
        name = "<buffer>";
    } else if (type == (jl_datatype_t*)jl_malloc_tag) {
        name = "<malloc>";
    } else if (jl_is_datatype(type)) {

        ios_t str_;
        ios_mem(&str_, 1024);
        JL_STREAM* str = (JL_STREAM*)&str_;

        jl_static_show(str, (jl_value_t*)type);

        name = string((const char*)str_.buf, str_.size);
        ios_close(&str_);

        self_size = (size_t)jl_datatype_size(type);
    }

    g_snapshot->node_ptr_to_index_map.insert(val,
            {a, g_snapshot->nodes.size()});
    count_nodes += 1;

    Node node{
        "object", // string type;
        name, // string name;
        (size_t)a, // size_t id;
        self_size, // size_t self_size;

        0, // int edge_count, will be incremented on every outgoing edge
        0, // size_t trace_node_id (unused)
        0  // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    g_snapshot->nodes.push_back(node);
}

// TODO: remove JL_DLLEXPORT
JL_DLLEXPORT void record_edge_to_gc_snapshot(char *type_description, jl_value_t *a, jl_value_t *b) {
    record_edge_to_gc_snapshot2(type_description, a, b, "");
}
JL_DLLEXPORT void record_edge_to_gc_snapshot2(char *type_description, jl_value_t *a, jl_value_t *b, char *fieldname) {
    if (!g_snapshot) {
        return;
    }

    record_node_to_gc_snapshot(a);
    record_node_to_gc_snapshot(b);

    auto from_node_idx = g_snapshot->node_ptr_to_index_map[a];

    g_snapshot->nodes[from_node_idx].type = type_description;
    g_snapshot->nodes[from_node_idx].edge_count += 1;

    g_snapshot->edges.push_back(Edge{"property",
                                    g_snapshot->names.find_or_create_string_id(fieldname), // name or index
                                    g_snapshot->node_ptr_to_index_map[b], // to
                                    // book-keeping
                                    g_snapshot->node_ptr_to_index_map[a],  // from
                                    });

    count_edges += 1;
}

void serialize_heap_snapshot(JL_STREAM *stream, HeapSnapshot &snapshot) {
    // mimicking https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/src/profiler/heap-snapshot-generator.cc#L2567-L2567
    jl_printf(stream, "{\"snapshot\":{");
    jl_printf(stream, "\"meta\":{");
    jl_printf(stream, "\"node_fields\":[\"type\",\"name\",\"id\",\"self_size\",\"edge_count\",\"trace_node_id\",\"detachedness\"],");
    jl_printf(stream, "\"node_types\":[");
    snapshot.node_types.print_json_array(stream, false);
    jl_printf(stream, ",");
    jl_printf(stream, "\"string\", \"number\", \"number\", \"number\", \"number\", \"number\"],");
    jl_printf(stream, "\"edge_fields\":[\"type\",\"name_or_index\",\"to_node\"],");
    jl_printf(stream, "\"edge_types\":[");
    snapshot.edge_types.print_json_array(stream, false);
    jl_printf(stream, ",");
    jl_printf(stream, "\"string_or_number\",\"node\"]");
    jl_printf(stream, "},\n"); // end "meta"
    jl_printf(stream, "\"node_count\":%zu,", snapshot.nodes.size());
    jl_printf(stream, "\"edge_count\":%zu", snapshot.edges.size());
    jl_printf(stream, "},\n"); // end "snapshot"

    jl_printf(stream, "\"nodes\":[");
    bool first_node = true;
    for (const auto &node : snapshot.nodes) {
        if (first_node) {
            first_node = false;
        } else {
            jl_printf(stream, ",");
        }
        // ["type","name","id","self_size","edge_count","trace_node_id","detachedness"]
        jl_printf(stream, "%zu", snapshot.names.find_or_create_string_id(node.type));
        jl_printf(stream, ",%zu", snapshot.names.find_or_create_string_id(node.name));
        jl_printf(stream, ",%zu", node.id);
        jl_printf(stream, ",%zu", node.self_size);
        jl_printf(stream, ",%zu", node.edge_count);
        jl_printf(stream, ",%zu", node.trace_node_id);
        jl_printf(stream, ",%d", node.detachedness);
        jl_printf(stream, "\n");
    }
    jl_printf(stream, "],\n");

    jl_printf(stream, "\"edges\":[");
    for (int i = 0; i < snapshot.edges.size(); ++i) {
        // Check that we constructed our nodes & edges correctly.
        assert(i == 0 ||
           snapshot.edges[i - 1].from_node <= snapshot.edges[i].from_node);

        const auto &edge = snapshot.edges[i];

        if (i != 0) {
            jl_printf(stream, ",");
        }
        jl_printf(stream, "%zu", snapshot.names.find_or_create_string_id(edge.type));
        jl_printf(stream, ",%zu", edge.name_or_index * k_node_number_of_fields);
        jl_printf(stream, ",%zu", edge.to_node * k_node_number_of_fields);
        jl_printf(stream, "\n");
    }
    jl_printf(stream, "],\n"); // end "edges"

    jl_printf(stream, "\"strings\":");

    snapshot.names.print_json_array(stream, true);

    jl_printf(stream, "}");
}
