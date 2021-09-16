#include "gc-heap-snapshot.h"

JL_DLLEXPORT void take_gc_snapshot() {
    jl_printf("HELLO\n");
    // Create the snapshot object

    // Enable GC Snapshotting

    // Do GC
    //     - which will callback into record_edge_to_gc_snapshot()...

    // When we return, the snapshot is full
    // Disable snapshotting

    // Dump the snapshot
}
