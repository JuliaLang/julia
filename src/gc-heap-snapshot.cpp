#include "gc-heap-snapshot.h"

#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>

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

struct Node {
    string type;
    string name;
    size_t id;
    size_t self_size;
    int edge_count;
    size_t trace_node_id;
    // whether the node is attached or dettached from the main application state
    // TODO: .... meaning not yet understood.
    // https://github.com/nodejs/node/blob/5fd7a72e1c4fbaf37d3723c4c81dce35c149dc84/deps/v8/include/v8-profiler.h#L739-L745
    int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
};

// Edges
// "edge_fields":
//   [ "type", "name_or_index", "to_node" ]

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
    StringTable node_types = {"node_type1", "node_type2"};
    StringTable edge_types = {"edge_type1", "edge_type2"};
    unordered_set<size_t> seen_node_ids;
};


// TODO: Do we need to refer to nodes by their index in the node array?
//size_t find_or_create_node_id(HeapSnapshot& snapshot, string key) {
//    return find_or_insert_iter(snapshot.nodes_map, key)->second;
//}

HeapSnapshot *g_snapshot = nullptr;

JL_DLLEXPORT void take_heap_snapshot(JL_STREAM *stream) {
    // Create the snapshot object
    //HeapSnapshot snapshot;
    //g_snapshot = &snapshot;
    if (!g_snapshot)
        g_snapshot = new HeapSnapshot();

    // Enable GC Snapshotting

    // Do GC
    //     - which will callback into record_edge_to_gc_snapshot()...

    // When we return, the snapshot is full
    // Disable snapshotting

    // Dump the snapshot
    serialize_heap_snapshot(stream, *g_snapshot);
    // TODO(PR): Put this back, but disabled for debugging
    //g_snapshot = nullptr;
}

JL_DLLEXPORT void record_node_to_gc_snapshot(jl_value_t *a) {
    auto val = g_snapshot->seen_node_ids.find((size_t)a);
    if (val != g_snapshot->seen_node_ids.end()) {
        return;
    }
    // Insert a new Node
    g_snapshot->seen_node_ids.insert(val, (size_t)a);

    Node node{
        "", // string type;
        "", // string name;
        (size_t)a, // size_t id;
        0, // size_t self_size;
        0, // int edge_count;
        0, // size_t trace_node_id;
        0 // int detachedness;  // 0 - unknown,  1 - attached;  2 - detached
    };
    g_snapshot->nodes.push_back(node);
}

// TODO: remove JL_DLLEXPORT
JL_DLLEXPORT void record_edge_to_gc_snapshot(jl_value_t *a, jl_value_t *b) {
    if (!g_snapshot) {
        return;
    }

    record_node_to_gc_snapshot(a);
    record_node_to_gc_snapshot(b);
    g_snapshot->edges.push_back(Edge{"property", (size_t)a, (size_t)b});
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
    jl_printf(stream, "\"string_or_number\",\"node\"],");
    jl_printf(stream, "\"trace_function_info_fields\":[],");
    jl_printf(stream, "\"trace_node_fields\":[],");
    jl_printf(stream, "\"sample_fields\":[],");
    jl_printf(stream, "\"location_fields\":[]");
    jl_printf(stream, "},\n"); // end "meta"
    jl_printf(stream, "\"node_count\":%zu,", snapshot.nodes.size());
    jl_printf(stream, "\"edge_count\":%zu,", snapshot.edges.size());
    jl_printf(stream, "\"trace_function_count\":0");
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
        jl_printf(stream, ",%zu", 0);//XXX); // self_size
        jl_printf(stream, ",%zu", 0);//XXX); // edge_count
        jl_printf(stream, ",%zu", 0);//XXX); // trace_node_id
        jl_printf(stream, ",%zu", 0);//XXX); // detachedness
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
        jl_printf(stream, ",%zu", edge.name_or_index);
        jl_printf(stream, ",%zu", edge.to_node);
        jl_printf(stream, "\n");
    }
    jl_printf(stream, "],\n"); // end "edges"

    jl_printf(stream, "\"strings\":");

    snapshot.names.print_json_array(stream, true);

    jl_printf(stream, "}");
}
