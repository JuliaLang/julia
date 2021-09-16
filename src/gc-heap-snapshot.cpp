#include "gc-heap-snapshot.h"

#include <vector>
#include <string>
#include <unordered_map>

using std::vector;
using std::string;
using std::unordered_map;

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

typedef unordered_map<string, size_t> MapType;    

class HeapSnapshot {
public:

// private:
    vector<Node> nodes;
    vector<Edge> edges;

    MapType names;
};


size_t find_or_create_string_id(HeapSnapshot& snapshot, string key) {
    auto &names = snapshot.names;

    auto val = names.find(key);
    if (val == names.end()) {
        val = names.insert(val, {key, names.size()});
    }
    return val->second;
}

HeapSnapshot *g_snapshot = 0;

JL_DLLEXPORT void take_gc_snapshot() {
    // Create the snapshot object
    HeapSnapshot snapshot;
    g_snapshot = &snapshot;

    // Enable GC Snapshotting

    // Do GC
    //     - which will callback into record_edge_to_gc_snapshot()...

    // When we return, the snapshot is full
    // Disable snapshotting

    // Dump the snapshot
    serialize_heap_snapshot(JL_STDERR, snapshot);
    g_snapshot = 0;
}

// TODO: remove JL_DLLEXPORT
JL_DLLEXPORT void record_edge_to_gc_snapshot(jl_value_t *a, jl_value_t *b) {
    if (!g_snapshot) {
        return;
    }

    g_snapshot->edges.push_back(Edge{"", (size_t)a, (size_t)b});

    jl_printf(JL_STDERR, "edge: %p -> %p\n", a, b);
}

void serialize_heap_snapshot(JL_STREAM *stream, HeapSnapshot &snapshot) {
    jl_printf(stream, "{");

    jl_printf(stream, "\"nodes\":[");
    bool first_node = true;
    for (const auto &node : snapshot.nodes) {
        if (!first_node) {
            jl_printf(stream, ",");
            first_node = false;
        }
        // ["type","name","id","self_size","edge_count","trace_node_id","detachedness"]
        jl_printf(stream, "%d", find_or_create_string_id(snapshot, node.type)); // type
        jl_printf(stream, ",%d", find_or_create_string_id(snapshot, node.name)); // name
        jl_printf(stream, ",%d", 0);//XXX); // id
        jl_printf(stream, ",%d", 0);//XXX); // self_size
        jl_printf(stream, ",%d", 0);//XXX); // edge_count
        jl_printf(stream, ",%d", 0);//XXX); // trace_node_id
        jl_printf(stream, ",%d", 0);//XXX); // detachedness
        jl_printf(stream, "\n");
    }
    jl_printf(stream, "],\n");

    jl_printf(stream, "\"edges\":[");
    bool first_edge = true;
    for (const auto &edge : snapshot.edges) {
        if (!first_edge) {
            jl_printf(stream, ",");
            first_edge = false;
        }
        // edge type
        jl_printf(stream, ",%d", find_or_create_string_id(snapshot, edge.type));
        // edge from
        jl_printf(stream, ",%d", edge.name_or_index);
        // edge to
        // TODO: don't print comma after the last
        jl_printf(stream, ",%d", edge.to_node);
        jl_printf(stream, "\n");
    }
    jl_printf(stream, "]");

    jl_printf(stream, "}");
}
