# Tools

Two tools: the discipline checker, which finds the stores of a program that
break the region rule before the program runs under regions, and the core
isolation for the paced measurements.

## The discipline checker

The checker runs a program under a hooked compiler. Region identity lives in
a side table, not in the runtime, so the checker runs on any julia whose
`Compiler` the hook fits, with no regions in use.

| File | Command | Prints |
| --- | --- | --- |
| [`hook_patch.py`](hook_patch.py) | `python3 hook_patch.py <workdir> [<julia>]` | builds a copy of `Compiler` with one hook after the optimizer passes into `<workdir>/Compiler`, and an environment that loads it into `<workdir>/env`. Prints the steps as it goes. Without `<julia>` it uses `julia +1.13`. |
| [`region_check.jl`](region_check.jl) | included by [`checker_run.jl`](checker_run.jl) | the checker: the pass that wraps every allocation with a registration and every reference store with a check, and the violation table by (parent type, child type). |
| [`checker_run.jl`](checker_run.jl) | `JULIA_LOAD_PATH=<workdir>/env:@stdlib julia checker_run.jl alloc\|clean <events>` | runs one model of `../bench` under the checker: the events processed, the instrumented methods, the registered objects, and `violations: N stores at M (parent type, child type) sites`, ranked. |

## The core isolation

| File | Command | Prints |
| --- | --- | --- |
| [`hil_isolation.sh`](hil_isolation.sh) | `sudo ./hil_isolation.sh on\|off`, `./hil_isolation.sh status`, `./hil_isolation.sh run CMD` | moves the CPUs of the measurement core (`CPUS`, default `13,29`) into an isolated cgroup partition, or back; `status` prints what holds now; `run` runs `CMD` inside the partition, pinned to `CORE` (default 29), with `SCHED_FIFO` when granted. |
