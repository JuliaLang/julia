#include "gc-heap-snapshot.h"

#include <vector>
#include <string>

using std::vector;
using std::string;



// Dump format:
// Nodes
// "node_fields":
//   [ "type", "name", "id", "self_size", "edge_count", "trace_node_id", "detachedness" ]

// Edges
// "edge_fields":
//   [ "type", "name_or_index", "to_node" ]

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
struct Edge {
};

class HeapSnapshot {
public:

private:
    vector<Node> nodes;
    vector<Edge> edges;
};







JL_DLLEXPORT void take_gc_snapshot() {
    jl_printf(JL_STDERR, "%s\n", "HELLO");
    // Create the snapshot object

    // Enable GC Snapshotting

    // Do GC
    //     - which will callback into record_edge_to_gc_snapshot()...

    // When we return, the snapshot is full
    // Disable snapshotting

    // Dump the snapshot
}

// TODO: remove JL_DLLEXPORT
JL_DLLEXPORT void record_edge_to_gc_snapshot(jl_value_t *a, jl_value_t *b) {
    jl_printf(JL_STDERR, "edge: %p -> %p\n", a, b);
}
