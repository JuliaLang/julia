# Process termination

A termination request (SIGTERM, a Windows close event, or SIGINT when Julia exits on it)
cancels the process root cancellation source. Every task governed by it gets a
`CancellationRequest` at its next cancellation point and unwinds. The main task then
returns, and the ordinary exit runs the `atexit` hooks and re-raises the signal.

## Code that never checks for cancellation

A loop that computes without ever yielding never reaches a cancellation point, so the
ordinary exit might never start. A watchdog thread outside Julia's thread pools covers
this case. It starts with the request and looks at each pool thread every 50 ms. If some
thread goes the whole grace period (2 s) without being seen to wait, the watchdog claims
the exit and runs the `atexit` hooks itself, on another thread outside the pools. This is
the fallback teardown. If that does not finish within 5 s, the watchdog re-raises the
signal anyway. If a collection is still waiting for the busy thread 1 s after the claim,
the hooks cannot run, and the watchdog re-raises the signal at once.

The watchdog sees a thread wait when either of these holds:

- the thread is sleeping in the scheduler when the watchdog looks;
- the thread's switch count moved since the last look. A thread bumps the count on each
  task switch, including a switch back to the running task, and on each pass of the
  scheduler's wait loop.

The count needs both parts. A task that calls `yield()` with nothing else to run switches
to itself, so its current task never changes. Each such `yield()` also wakes the idle
threads, which then spin in the scheduler without sleeping and without switching tasks.

So a cleanup that waits in the scheduler (sleeping, doing I/O, waiting for tasks, or
yielding) is not cut off, however long it takes, as long as no other thread is busy. A
busy thread anywhere triggers the fallback, which then cuts off the waiting cleanups too.
Waits the scheduler does not see still look busy: a blocking foreign call, a runtime lock,
or a thread stopped at a GC safepoint. If no thread is busy and the main task never returns
(it waits in a shielded scope, or loops on `yield()` for ever), nothing fires. A second
signal ends the process at once, and so does a supervisor's SIGKILL.

The fallback teardown runs while busy threads keep running. A hook that waits for a task
in a thread pool can wait until the deadline, since every thread that could run it may be
busy. Base's own hooks skip such waits when they run outside the thread pools.

## TLA+ model

[`termination/TermExit.tla`](https://github.com/JuliaLang/julia/blob/master/doc/src/devdocs/termination/TermExit.tla)
models the watchdog against threads that compute without yielding, poll with `yield()`,
switch between two tasks, wait for an event, or sit idle. Each scheduler wait is split
into the steps the C code takes, and the watchdog reads each thread's count and sleep
state as separate steps. Time is counted in polls, and threads take the steps they owe
within `Lag` polls. That is an assumption about the threads, which the model makes
explicit. The model checker picks each thread's kind of task and explores every
interleaving. It checks that:

- the fallback runs only when some thread is busy (`NoFalseFire`);
- the fallback teardown finishes before its deadline (`NoDeadlineKill`);
- some exit always starts, and without busy code it is the ordinary one.

`TermExit.cfg` is the design above, with two threads, a grace of 4 polls and threads that
step within a poll. TLC finds no violation. The grace needs that margin. A thread can take
two steps between bumps of its count, for example computing and then entering a wait. If
each step lands just after the watchdog reads the thread, the thread goes three polls
unseen, so a grace of 3 polls fires. In general the grace has to exceed the time between
count bumps, plus one poll. With 2 s against 50 ms polls, a thread may run about 1.9 s
between waits.

The other configs each change one thing, and TLC finds a counterexample:

- `TermExitLag.cfg` lets threads go two polls without a step they owe, so the gap between
  bumps grows past the grace.
- `TermExitTask.cfg` compares the current task instead of a count. A switch to another
  task and back between two reads looks like no switch, and so does a `yield()` to the
  running task.
- `TermExitSwitch.cfg` counts task switches but not scheduler passes. A thread on its way
  to sleep, or kept awake by `yield()` in another task, is in the scheduler loop and is
  not seen until it parks.
- `TermExitHookWait.cfg` lets a hook wait for its listener task in the fallback teardown.
  When every default-pool thread is busy, it waits until the deadline, even with an
  interactive thread free.

To reproduce, with [`tla2tools.jar`](https://github.com/tlaplus/tlaplus/releases):

```sh
cd doc/src/devdocs/termination
java -cp tla2tools.jar tlc2.TLC -config TermExit.cfg TermExit.tla
```

Three threads make the state space too large to check quickly, so the configs use two.
The model leaves out weak memory, task migration, waits the scheduler does not see, the
hooks of the ordinary exit, and what the fallback teardown does to the threads that keep
running.
