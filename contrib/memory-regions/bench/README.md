# Benchmarks

Every script needs a julia built from this branch, except where a line says
otherwise. Run a script from this directory. Each script prints its result to
stdout; when `REGIONS_TSV` names a file, the script also appends one row of
numbers to that file ([`report.jl`](report.jl)). `../results/run_all.sh` runs
every measurement below and `../results/plot.py` draws from the rows.

## Programs

| File | Command | Prints |
| --- | --- | --- |
| [`unit_costs.jl`](unit_costs.jl) | `julia unit_costs.jl [N]` | one TSV row per unit cost of the runtime (store, window pair, switch, construction, alloc, reset slice, stock mark), min of N. `julia unit_costs.jl stock [N]` runs only the rows that need no region entry point, so it also runs on a vanilla julia. |
| [`gcbench.sh`](gcbench.sh) | `gcbench.sh <vanilla julia> <region julia> <GCBenchmarks checkout> [rounds]` | the wall time of every GCBenchmarks run, both binaries in every round: six serial benchmarks on one thread, five multithreaded ones on four. The zero-cost sweep. |
| [`yardstick.jl`](yardstick.jl) | `julia yardstick.jl alloc\|pooled <events>` | the per-event latency distribution of one model variant under the stock collector, against the 100 µs pacing target. |
| [`tail.jl`](tail.jl) | `julia tail.jl baseline\|regions <events>` | the tail latencies of the pooled model: `baseline` leaves the scratch to the collector, `regions` resets the Event region after each event. |
| [`paced.jl`](paced.jl) | `julia --heap-size-hint=128M paced.jl baseline <events>`, `julia paced.jl regions <events>` | the lateness of every event against its 100 µs slot on the wall clock, and the miss count. |
| [`endurance.jl`](endurance.jl) | `julia endurance.jl <events>` | one sample per 100k events of wall time, high-water RSS, `gc_live_bytes`, the pages the reset returned, and the miss count. 18000000 events is 30 minutes. |
| [`census.jl`](census.jl) | `julia census.jl <mode> <events> [every] [K] [W] [B]` | the pause of a region census against a full sweep, one 27-column TSV row per run. Modes: `scoped`, `coop`, `pooled`, `full`, `auto`, `batch`, `autopool`, `real`. |
| [`census_bound.jl`](census_bound.jl) | `julia census_bound.jl` | the region page count per round with the open-region census disarmed and armed; exits 1 when the two accumulators differ or the region is quarantined. |
| [`native.jl`](native.jl) | `julia native.jl region\|stock <events> [census_every] [W]` | the wall time, RSS, and census count of a region-native model: packets allocated at send, dropped at delivery, no pool. |
| [`native.cpp`](native.cpp) | `g++ -O2 -std=c++17 -o native native.cpp && ./native <events> [W]` | the same model in C++ with `new`/`delete` per event and per packet; the wall time and RSS from `getrusage`. |

## Files that the programs include

| File | Content |
| --- | --- |
| [`kernel.jl`](kernel.jl) | the minimal sequential kernel: modules, gates, an event queue ordered by time. |
| [`model_alloc.jl`](model_alloc.jl) | the allocating model, the baseline: a source, a relay chain, a sink that keeps a record per delivery. |
| [`model_clean.jl`](model_clean.jl) | the same traffic written under the region rules: the packet in the Simulation region, transients in the Event region. |
| [`model_pooled.jl`](model_pooled.jl) | the same traffic with every per-event allocation removed by hand: the benefit upper bound. |
| [`model_scratch.jl`](model_scratch.jl) | a pooled message with ordinary allocating scratch in the sink: the code the Event region reset reclaims. |
| [`report.jl`](report.jl) | `tsv_row(names, values)`: appends one row to the file that `REGIONS_TSV` names, with a header row into a new file. |
