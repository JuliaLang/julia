# Demonstrators

Every program runs the same code twice, once with regions and once under the
stock collector, and prints the two side by side. Every program needs a julia
built from this branch. Run a program from this directory. When `REGIONS_TSV`
names a file, the program also appends one row of numbers to that file;
`../results/run_all.sh` collects the rows and `../results/plot.py` draws them.

## Programs

| File | Command | Prints |
| --- | --- | --- |
| [`bt_solver.jl`](bt_solver.jl) | `julia bt_solver.jl` | Demonstrator A: backtracking graph colouring. Wall time, stock collection count, GC time, and peak RSS per run, regions against stock, interleaved A/B/A/B. |
| [`pathtrace.jl`](pathtrace.jl) | `julia -t4 pathtrace.jl` | Demonstrator B: a parallel path tracer, one sibling leaf per worker thread. The same table, plus a bit-exact check that both images are equal. |
| [`optimistic_bst.jl`](optimistic_bst.jl) | `julia -t4 optimistic_bst.jl` | Demonstrator C: an optimistic concurrent persistent BST, lost attempts reset in O(1). The same table, plus the abort count. |
| [`dmr.jl`](dmr.jl) | `julia -t4 dmr.jl` | Demonstrator D: optimistic parallel Delaunay mesh refinement, a lost speculation reset in O(1). The same table, plus a check that both meshes are equal. |
| [`showcase_binarytree.jl`](showcase_binarytree.jl) | `julia showcase_binarytree.jl [stock\|region] [depth]` | the GCBench binary tree in one mode: the wall time, the collection count, the GC time, and the peak RSS. |
| [`showcase_linkedlist.jl`](showcase_linkedlist.jl) | `julia showcase_linkedlist.jl [stock\|region] [mb]` | GCBenchmarks' linked list in one mode: the wall time, the collection count, the GC time, and the peak RSS. |
| [`showcase_tree.jl`](showcase_tree.jl) | `julia -t4 showcase_tree.jl` | the region tree: N sibling leaves, one per worker task, against the same workload without regions. The wall time and the collection count of both, and a check that both results are equal. |

## Files that the programs include

| File | Content |
| --- | --- |
| [`demo_common.jl`](demo_common.jl) | the shared harness: `measure` records the per-run deltas of `Base.gc_num` and the peak RSS; the interleaved A/B/A/B driver; `report_table`; `tsv_row`. |
| [`dmr_core.jl`](dmr_core.jl) | the sequential Bowyer-Watson core of Demonstrator D. |
