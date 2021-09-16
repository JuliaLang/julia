#include "gc-heap-snapshot.h"

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
