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

`jl_wakeup_threadpool` wakes at most one sleeping thread in the pool per insert.
It tries the thread that went to sleep most recently first, since its core is
likely to be warm.

## Searchers

A thread that fails to pop a task can become a searcher: it keeps polling the
queues until the sleep threshold passes, then goes to sleep. Each pool counts its
searchers in `n_spinning`. A thread gets a searcher slot while
`2 * n_spinning < pool size`, so at most half the pool (rounded up) polls at
once. A thread that does not get a slot goes to sleep right away.

Each pool also counts the tasks in its multiqueue in `n_ready`. `multiq_insert`
and `multiq_deletemin` update it under the heap lock, so a task is counted before
any thread can pop it. `jl_wakeup_threadpool` wakes a thread only when
`n_spinning < n_ready`, that is, when there are more pending tasks than
searchers looking for them.

A woken thread starts as a searcher. `wake_thread` increments `n_spinning` for
the target before the compare-and-swap on its `sleep_check_state`, and the woken
thread takes over that slot. If the compare-and-swap fails, the waker decrements
`n_spinning` again.

Two rules make it safe to skip a wake:

1. A searcher that goes to sleep releases its slot before step 1. An enqueuer
   that skipped its wake because it counted this searcher is then ordered before
   the check in step 3, which finds the task.
2. A searcher that leaves `jl_task_get_next` without reaching step 3 runs the
   wake check again if it held the pool's last slot. This covers finding a task,
   running the `^C` dispatch pass (which can block on a lock), and unwinding an
   exception.

A waker whose compare-and-swap fails follows rule 2 as well, since its temporary
increment may have made another enqueuer skip its wake. `drain_pool_wakeups`
repeats the check until no more slots are released this way.

`trypoptask`, `checkempty` and libuv callbacks run Julia code that can throw,
including during the sleep sequence. The exception handlers restore
`sleep_check_state` and `n_threads_running`, release the slot, and run the wake
check again.

## TLA+ model

[`scheduler-wakeup/SchedulerWake.tla`](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/scheduler-wakeup/SchedulerWake.tla)
models this protocol with each step as an atomic action. `NoLostWakeup` states
that a pool with queued tasks always has a thread that will run them, and
`SpinCountOK` that `n_spinning` matches the slots held. `MCFixed` checks two
threads sharing a pool plus a producer in a second pool. To run it with
[`tla2tools.jar`](https://github.com/tlaplus/tlaplus/releases):

```sh
cd doc/src/devdocs/scheduler-wakeup
java -cp tla2tools.jar tlc2.TLC -config MCFixed.cfg MCFixed.tla
```
