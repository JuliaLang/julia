# Task scheduler wakeups

This page describes how threads in the task scheduler go to sleep and how they
are woken when work arrives. The code is in
[`src/scheduler.c`](https://github.com/JuliaLang/julia/blob/master/src/scheduler.c).

## Sleeping and waking

A thread that finds no work in `jl_task_get_next` goes to sleep in this order:

1. set `sleep_check_state = sleeping`,
2. `jl_fence()`,
3. check the queue again, and stop if work appeared,
4. decrement `n_threads_running`,
5. wait on its condition variable while `sleep_check_state` is still `sleeping`.

A thread that enqueues a task calls `jl_fence()` after the insert and then reads
the `sleep_check_state` of the threads in the task's pool. The two fences
(`[^store_buffering_1]` in `scheduler.c`) ensure that either the enqueuer sees
the sleeping state and wakes that thread, or the check in step 3 sees the new
task.

`jl_wakeup_threadpool` wakes at most one sleeping thread in the pool per insert,
and only when needed (see below). It tries the thread that went to sleep most
recently first, since its core is likely to be warm.

Each pool counts its sleeping threads in `n_sleeping`. A thread increments it
before step 1, and whichever thread sets its `sleep_check_state` back to not
sleeping decrements it. An enqueuer that reads 0 after its fence skips the scan:
the fences order it before the check in step 3 of any thread that is about to
sleep. This keeps enqueues cheap while the whole pool is busy.

## Searchers

A thread that fails to pop a task can become a searcher: it keeps polling the
queues until the sleep threshold passes, then goes to sleep. Each pool counts its
searchers in `n_spinning`. A thread gets a searcher slot while fewer than half
the pool (rounded up) holds one. A thread whose last search was useful may take
any free slot. A search is useful when it found a task, or when a task woke the
thread less than the sleep threshold after it started to sleep, so that
searching would have caught it. A search that reaches the threshold without
finding work is not. With `JULIA_THREAD_SLEEP_THRESHOLD=infinite` threads never
sleep, so any thread may take a free slot. A thread that does not get a slot
goes to sleep right away, still checking the queues once more first (step 3).

A woken thread starts as a searcher. `wake_thread` increments `n_spinning` for
the target before the compare-and-swap on its `sleep_check_state`, and the woken
thread takes over that slot. If the compare-and-swap fails, the waker decrements
`n_spinning` again.

An enqueue wakes a thread only if `n_spinning` is 0. A searcher that finds a
task releases its slot. If it held the pool's last slot, it then checks the
queues and wakes one more thread if they are not empty and nobody else
searches. A burst of tasks from one thread therefore ramps up one wake at a
time.

These rules make it safe to skip a wake:

1. A searcher that goes to sleep releases its slot before step 1. An enqueuer
   that skipped its wake because it counted this searcher is then ordered before
   the check in step 3, which finds the task. The thread then stops going to
   sleep. If its next pop gets a task, it follows rule 2 as if it had held the
   last slot. If not, it takes a slot again, or is denied one because other
   searchers hold the slots.
2. The last searcher to find a task checks the queues after releasing its slot,
   as described above. The fences order this check against the enqueuer's read
   of `n_spinning`.
3. The last searcher, or a thread that owes a wake under rule 1, that leaves
   `jl_task_get_next` in any other way wakes a thread if `n_spinning` is 0 and
   any thread sleeps. This covers running the `^C` dispatch pass (which can
   block on a lock) and unwinding an exception.

A waker whose compare-and-swap fails follows rule 3 as well, since its temporary
increment may have made another enqueuer skip its wake. `drain_pool_wakeups`
repeats the check until no more slots are released this way.

`trypoptask`, `checkempty` and libuv callbacks run Julia code that can throw,
including during the sleep sequence. The exception handlers restore
`sleep_check_state` and `n_threads_running`, release the slot, and run the wake
check again. An error from the queue check in rule 2 is ignored, since the
thread already took a task: the task runs, and another thread is woken.

## TLA+ model

[`scheduler-wakeup/SchedulerWake.tla`](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/scheduler-wakeup/SchedulerWake.tla)
models this protocol with each step as an atomic action. It checks four
invariants:

- `NoLostWakeup`: a pool with queued tasks always has a thread that will run
  them.
- `NoStrandedWork`: if a pool has queued tasks and a parked thread, some thread
  in the pool will still look at the queues.
- `SpinCountOK`: `n_spinning` matches the slots held.
- `SleepCountOK`: `n_sleeping` counts every sleeping thread.

`MCFixed` checks two threads sharing a pool plus a producer in a second pool.
To run it with
[`tla2tools.jar`](https://github.com/tlaplus/tlaplus/releases):

```sh
cd doc/src/devdocs/scheduler-wakeup
java -cp tla2tools.jar tlc2.TLC -config MCFixed.cfg MCFixed.tla
```
