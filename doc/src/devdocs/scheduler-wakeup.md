# Task scheduler wakeups

This page documents the sleep/wake handshake used by Julia's task scheduler:
why `jl_wakeup_threadpool` wakes at most one worker per multiqueue insert, and
how searcher accounting decides when to skip the wake entirely.

## Waking one worker per insert

Every multiqueue insert used to call `jl_wakeup_thread(-1)`, which signals
every sleeping thread. A burst of spawns produced an ``O(nthreads)`` wake storm
per insert (JuliaLang/julia#61820, JuliaLang/julia#50425): every idle worker
woke, raced for the one new task, and went back to sleep.

`jl_wakeup_threadpool(tpid)` wakes at most one sleeping worker *in the task's
pool*. Correctness rests on the store-buffering fence (`[^store_buffering_1]`
in [`src/scheduler.c`](https://github.com/JuliaLang/julia/blob/master/src/scheduler.c))
that the broadcast path also relied on. The per-thread invariant is:

> A worker is observed `sleeping` by the waker, **or** it has not yet committed
> to its sleep transition and is therefore guaranteed to re-check its queue and
> find the freshly-inserted task.

The consumer's sleep transition in `jl_task_get_next` runs in this order
(searchers first *release* their slot — see below):

1. **publish** `sleep_check_state = sleeping`,
2. `jl_fence()`,
3. **recheck** the queue (`check_empty`); abort the sleep if work appeared,
4. **retire** from the running count (decrement `n_threads_running`),
5. **park** on the condition variable while `may_sleep` holds.

The enqueuer fences after the insert, then inspects each candidate's
`sleep_check_state`. Because *publish* precedes *recheck*, the enqueuer either
sees `sleeping` and wakes the worker, or the worker observes the new task
itself.

### Why the scan cannot be skipped on `n_threads_running`

Skipping the scan when `n_threads_running >= jl_n_threads` ("everything is
already running, nothing is parked") is unsound for two independent reasons:

1. **The count lags the per-thread state.** It is decremented at *retire*,
   after the re-check: a worker between *publish* and *retire* may have read
   its queue as empty while still counted as running, so an enqueuer consulting
   the count skips a wake that worker needs.
2. **The count is global but the wake is pool-local.** Busy workers in one pool
   keep the count high while another pool is entirely parked, so a cross-pool
   insert (an `:interactive` task spawned from a `:default` worker) would be
   dropped and the task stranded.

`sleep_check_state` has neither problem: it is set at *publish*, before the
danger window, and it is inspected per pool.

## Searcher accounting

A thread that fails to pop work may become a *searcher*, polling the queues
until the sleep threshold instead of parking immediately. A searcher slot is
granted while `2 * n_spinning < pool size`, bounding searchers at half the pool
(rounded up); a thread denied a slot parks without polling.

`jl_wakeup_threadpool` skips its wake while `n_spinning >= n_ready`: committed
searchers already cover the pending tasks. `n_ready` is maintained by the queue
implementation (`jl_sched_nready_inc/dec` from `multiq_insert` and
`multiq_deletemin`, both under the heap lock, so a task's decrement cannot
precede its increment). A burst of `k` enqueues wakes up to `k` workers in
parallel; a single enqueue with an active searcher wakes nobody.

A wake also starts the woken thread as a searcher, so wakes in flight count as
supply at the gate — a woken thread takes microseconds to start searching, and
without this every enqueue in that window wakes another thread. `wake_thread`
increments `n_spinning` before the sleep-state CAS and undoes it on failure, so
only the CAS winner's increment stands. The woken thread takes the slot at the
raced-detection sites that consume the in-flight `n_threads_running` increment
and releases it through the normal searcher exits. Pre-accounting is valid only
where a `jl_task_get_next` frame will consume the slot, so `surprise_wakeup`,
called from task teardown, does not pre-account.

Two orderings make the gate sound:

1. **Release before publish.** A searcher heading to sleep decrements
   `n_spinning` *before* its `sleep_check_state = sleeping` store, fence and
   re-check, pairing with the enqueuer's *insert, fence, read `n_spinning`*
   sequence (`[^store_buffering_1]`): an enqueuer that skips the wake on a
   stale count is ordered before the re-check, which then observes the task.
2. **The last searcher out wakes a successor if work remains** (the *exit
   handoff*), on every exit that skips the parking re-check — leaving with
   work, and unwinding. A task whose wake was suppressed on this searcher's
   account gets a successor instead of stranding.

The second rule also applies when a wake attempt loses its sleep-state CAS.
Its temporary increment may already have suppressed another enqueue's wake,
even though the target resumed on its own and never acquired that slot.
`unaccount_searcher` records a pool handoff when rollback removes the last
slot. `drain_pool_wakeups` drains these requests through the count gate, retrying
iteratively if a wake scan incurs another rollback. Dead threads are excluded
before pre-accounting so scanning them cannot repeatedly create new handoffs.

### The unwind exit

An exception is the third way out of `jl_task_get_next`, besides finding work
and parking: `trypoptask`/`checkempty` can throw, and a SIGINT delivered to a
searching thread surfaces there. It is a routine exit, not a failure path —
every interactive Ctrl-C is an `InterruptException` thrown from a safepoint
inside the sleep transition — so its bookkeeping must be exact. The handlers
restore the sleep transition, release the slot, and run the exit handoff, which
an unwinding thread owes because it never performs the post-fence re-check.

## TLA+ model

[`scheduler-wakeup/`](https://github.com/JuliaLang/julia/tree/master/doc/src/devdocs/scheduler-wakeup)
makes the argument above machine-checkable.
[`SchedulerWake.tla`](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/scheduler-wakeup/SchedulerWake.tla)
models the sleep transition as the steps listed above, with the slot release as
a separate atomic step from the sleeping-store so TLC explores the window
between them, plus the count gate, the wake-as-searcher increment, and the
unwind exit. It also splits an additional competing targeted wake into
pre-accounting, CAS success or rollback, and a deferred rollback handoff, so
TLC explores the temporary overcount rather than treating it as atomic.
An unwound worker sits in an `"outside"` state that
`NoLostWakeup` counts as unable to service work; a pool whose workers all
unwound is excused, since those workers are running user code rather than
parked. `MCFixed` instantiates the model with two workers sharing a pool and a
cross-pool producer.

TLC explores the full state space with no deadlock, no `NoLostWakeup`
violation, and `SpinCountOK` (the counter matches held and provisional slots).
Weakening the model breaks it: releasing the slot only at park commit, or
omitting the unwind handoff, each make `NoLostWakeup` fail. Setting
`RollbackHandsOff = FALSE` also violates `NoLostWakeup`: the temporary slot
can suppress both an enqueue's wake and the last real searcher's handoff,
then disappear without leaving anyone responsible for the queued work.

The model checks the protocol, not the C code. Every scheduler exit path in C
(found work, park, unwind) maps to a model action, and the model assumes
sequentially-consistent atomic steps — the argument that the C11 fences realize
them is the store-buffering pairing documented in `src/scheduler.c`.

To reproduce, with [`tla2tools.jar`](https://github.com/tlaplus/tlaplus/releases):

```sh
cd doc/src/devdocs/scheduler-wakeup
java -cp tla2tools.jar tlc2.TLC -config MCFixed.cfg MCFixed.tla
```
