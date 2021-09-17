#include "gc-heap-snapshot.h"

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
    size_t id;
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
    string type;
    size_t name_or_index; // essentially 'from'
    size_t to_node;
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
            jl_printf(stream, "\"%s\"", str.c_str());
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
    jl_value_t* type = jl_typeof(a);

    size_t self_size = 1;
    string name = "<missing>";
    //self_size = jl_f_sizeof(a);

    if (jl_is_datatype(a)) {
        self_size = (size_t)jl_datatype_size(type);
        name = jl_typeof_str(a);
    }

    // // Copied from jl_static_show_x_:
    // if ((uintptr_t)type < 4096U) {
    //     // Handle non-julia values:
    //     // TODO:  sprintf the type pointer
    //     //name = sprintf(..., "<?::%p>", (void*)type);
    //     name = "<?::unknown>";
    // } else if ((uintptr_t)a < 4096U) {
    //     // TODO: understand this case?
    //     // n += jl_printf(out, "<?#%p::", (void*)v);
    //     // n += jl_static_show_x(out, (jl_value_t*)vt, depth);
    //     // n += jl_printf(out, ">");
    // }
    // else if (v == (jl_value_t*)jl_simplevector_type) {
    //     //n += jl_printf(out, "Core.SimpleVector");
    // }
    // else if (v == (jl_value_t*)jl_typename_type) {
    //     //n += jl_printf(out, "Core.TypeName");
    // }
    // else if (v == (jl_value_t*)jl_symbol_type) {
    //     //n += jl_printf(out, "Symbol");
    // }
    // else if (v == (jl_value_t*)jl_methtable_type) {
    //     //n += jl_printf(out, "Core.MethodTable");
    // }
    // else if (v == (jl_value_t*)jl_any_type) {
    //     //n += jl_printf(out, "Any");
    // }
    // else if (v == (jl_value_t*)jl_type_type) {
    //     //n += jl_printf(out, "Type");
    // }
    // else if (vt == jl_method_type) {
    //     //jl_method_t *m = (jl_method_t*)v;
    //     //n += jl_static_show_func_sig(out, m->sig);
    // } else {
    //     // Handle julia values:
    //     //jl_printf(JL_STDERR, "value: %p\n", a);
    //     //jl_printf(JL_STDERR, "type: %p\n", type);
    //     //jl_static_show(JL_STDERR, a);
    //     self_size = (size_t)jl_datatype_size(type);
    //     name = "name";
    // }


    g_snapshot->node_ptr_to_index_map.insert(val,
            {a, g_snapshot->nodes.size()});
    count_nodes += 1;

    Node node{
        "object", // string type;
        name, // string name;
        (size_t)a, // size_t id;
        // TODO: This currently segfaults:
        self_size, // size_t self_size;
        //0, // size_t self_size;

        0, // int edge_count;
        0, // size_t trace_node_id;
        0 // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    g_snapshot->nodes.push_back(node);
    //return &g_snapshot->nodes.back();
}

// TODO: remove JL_DLLEXPORT
JL_DLLEXPORT void record_edge_to_gc_snapshot(jl_value_t *a, jl_value_t *b) {
    if (!g_snapshot) {
        return;
    }

    record_node_to_gc_snapshot(a);
    record_node_to_gc_snapshot(b);

    auto from_node_idx = g_snapshot->node_ptr_to_index_map[a];
    //cout << from_node_idx << endl;

    g_snapshot->nodes[from_node_idx].edge_count += 1;
    g_snapshot->edges.push_back(Edge{"property",
                                    g_snapshot->node_ptr_to_index_map[a],
                                    g_snapshot->node_ptr_to_index_map[b]});

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
    bool first_edge = true;
    for (const auto &edge : snapshot.edges) {
        if (first_edge) {
            first_edge = false;
        } else {
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
