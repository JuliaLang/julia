# Task scheduler wakeups

This page documents the sleep/wake handshake used by Julia's task scheduler and,
in particular, why `jl_wakeup_threadpool` may wake only a *single* worker per
multiqueue insert instead of broadcasting to every thread.

## The problem

Every `@spawn` that lands in the multiqueue used to call `jl_wakeup_thread(-1)`,
which loops over *all* threads and issues a `uv_cond_signal` to each sleeping
one. On an oversubscribed machine a burst of spawns therefore produces an
``O(nthreads)`` "wake storm" per insert (JuliaLang/julia#61820,
JuliaLang/julia#50425): every idle worker is woken, races for the one new task,
and all but one immediately go back to sleep.

`jl_wakeup_threadpool(tpid)` replaces that broadcast. A multiqueue insert wakes
at most one sleeping worker *in the task's pool*. A burst of ``N`` inserts wakes
up to ``N`` workers. There is no peer-to-peer cascade: a woken worker simply
pops its task and runs it.

## Why waking one worker is enough

Correctness rests on the same store-buffering fence (`[^store_buffering_1]` in
[`src/scheduler.c`](https://github.com/JuliaLang/julia/blob/master/src/scheduler.c))
that the broadcast path relied on. The relevant per-thread invariant is:

> A worker is observed `sleeping` by the waker, **or** it has not yet committed
> to its sleep transition and is therefore guaranteed to re-check its queue and
> find the freshly-inserted task.

The consumer's sleep transition in `jl_task_get_next` is, in order:

1. publish `sleep_check_state = sleeping`,
2. `jl_fence()`,
3. re-check the queue (`check_empty`); abort the sleep if work appeared,
4. decrement `n_threads_running`,
5. park on the condition variable while `may_sleep` holds.

The enqueuer fences after the insert and then inspects each candidate's
`sleep_check_state`. Because the consumer publishes `sleeping` in step 1 — before
it re-checks the queue in step 3 — the enqueuer either sees `sleeping` (and wakes
the worker) or the worker will itself observe the new task in step 3. Either way
the task is serviced.

## Why the wake must *not* be skipped using `n_threads_running`

A tempting optimization is to skip the scan entirely when
`n_threads_running >= jl_n_threads` ("everything is already running, nothing is
parked"). This is **unsound**, for two independent reasons:

1. **The count lags the per-thread state.** `n_threads_running` is decremented in
   step 4, *after* the consumer has already re-checked its queue in step 3. In
   the window between steps 1 and 4 a worker has published `sleeping` and may
   have already read its queue as empty, yet is still counted as running. An
   enqueuer that consults the count therefore reads stale "all running"
   information and skips a wake the worker actually needs.

2. **The count is global but the wake is pool-local.** Busy workers in one pool
   keep `n_threads_running` high while another pool is entirely parked. A
   cross-pool insert (e.g. spawning an `:interactive` task from a `:default`
   worker) would then be dropped, stranding the task indefinitely.

Scanning `sleep_check_state` avoids both problems: that flag is set in step 1,
*before* the re-check, so a scan never misses a worker in the danger window, and
it is inspected per-pool.

## TLA+ model

The directory [`scheduler-wakeup/`](https://github.com/JuliaLang/julia/tree/master/doc/src/devdocs/scheduler-wakeup)
contains a small TLA+ model of this handshake that makes the argument above
machine-checkable.

* [`SchedulerWake.tla`](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/scheduler-wakeup/SchedulerWake.tla)
  models the sleep transition as the same discrete steps listed above, so the
  model checker explores every interleaving of that window against an enqueuer
  that wakes one worker by scanning `sleep_check_state`.
* `MCFixed` — a concrete instance (two workers sharing a pool, plus a cross-pool
  producer). TLC explores the full state space with no deadlock and no
  `NoLostWakeup` violation.

To reproduce, with [`tla2tools.jar`](https://github.com/tlaplus/tlaplus/releases):

```sh
cd doc/src/devdocs/scheduler-wakeup
java -cp tla2tools.jar tlc2.TLC -config MCFixed.cfg MCFixed.tla
```

Toggling the model back to the unsound `n_threads_running` short-circuit (by
making `Wakeup` return early when `nrun >= N`) makes TLC report a `NoLostWakeup`
violation, confirming the danger window described above is real.

## The event loop thread

Outside a threaded region, only one thread (`io_loop_tid`, normally thread 0)
runs the libuv event loop. When it has nothing else to do it publishes
`sleeping`, runs `uv_run(loop, UV_RUN_ONCE)` under the iolock (`jl_uv_mutex`),
and parks on its condition variable if the loop is idle afterwards (`uv_run`
returned 0) and nobody woke it in the meantime.

Other threads arm libuv handles (`uv_poll_start`, `uv_timer_start`, ...) under the
iolock, but they don't run the loop to get those handles serviced. A thread that
goes to sleep therefore wakes `io_loop_tid` unless it can tell that the loop has
nothing to do: it takes the iolock with a trylock, checks whether the loop is
alive, and releases it without running finalizers. If the iolock is busy, it
wakes the event loop thread.

Skipping the wake whenever the iolock is held, and leaving it to the holder,
is not enough. `JL_UV_UNLOCK` runs pending finalizers, and finalizers of
libuv-backed objects (`uvfinalize`) take the iolock. After the event loop
thread's unlock following `uv_run` releases the iolock, another thread can arm a
handle. If a finalizer then reacquires the iolock before that thread checks its
owner, the thread would skip the wake, and the event loop thread subsequently
parks based on its earlier idle result: nobody runs the loop again.

[`IoLoopWake.tla`](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/scheduler-wakeup/IoLoopWake.tla)
models this handoff outside threaded regions, including iolock contention and
finalizers that take the iolock before the loop runs and after its unlock. It
abstracts away blocking inside `uv_run` and shutdown, and checks the absence of
globally stranded armed handles, not eventual IO service while another thread
keeps running. With `IoLoopWake.cfg`, TLC finds no `NoLostWakeup` violation for
the given bounds; `IoLoopWakeUnfixed.cfg` skips the wake whenever the iolock is
held and produces a counterexample:

```sh
cd doc/src/devdocs/scheduler-wakeup
java -cp tla2tools.jar tlc2.TLC -config IoLoopWake.cfg IoLoopWake.tla
java -cp tla2tools.jar tlc2.TLC -config IoLoopWakeUnfixed.cfg IoLoopWake.tla
```
