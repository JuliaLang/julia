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
  model checker explores every interleaving of the steps 1–5 window against an
  enqueuer. A `CONSTANT EarlyOut` toggles between the unsound count-based
  short-circuit and the shipped scan.
* `MCBuggy` (`EarlyOut = TRUE`) — TLC reports a violation of the `NoLostWakeup`
  invariant: a cross-pool insert reads "all running", skips the wake, and the
  target worker parks with its task still queued.
* `MCFixed` (`EarlyOut = FALSE`) — TLC explores the full state space with no
  deadlock and no `NoLostWakeup` violation.

To reproduce, with [`tla2tools.jar`](https://github.com/tlaplus/tlaplus/releases):

```sh
cd doc/src/devdocs/scheduler-wakeup
java -cp tla2tools.jar tlc2.TLC -config MCBuggy.cfg MCBuggy.tla   # finds the lost wakeup
java -cp tla2tools.jar tlc2.TLC -config MCFixed.cfg MCFixed.tla   # passes
```
