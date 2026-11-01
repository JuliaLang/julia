------------------------------ MODULE TermExit ------------------------------
(***************************************************************************)
(* A TLA+ model of the watchdog that ends the process when a termination  *)
(* request (SIGTERM, or SIGINT with exit-on-sigint) is not acted on.       *)
(*                                                                         *)
(* The request cancels the process root source, and the ordinary exit runs *)
(* once the main task returns. A watchdog thread outside the thread pools  *)
(* polls the pool threads. If one goes the whole grace period without     *)
(* being seen to wait, the watchdog claims the exit and runs the atexit    *)
(* hooks on another thread (the fallback teardown), and ends the process   *)
(* if that misses its own deadline.                                        *)
(*                                                                         *)
(* `Watch` selects what the watchdog compares at each poll:                *)
(*                                                                         *)
(*   "task"    the thread's current task (an earlier version of the PR)    *)
(*   "switch"  a per-thread count bumped by every task switch, including   *)
(*             a switch to the running task itself                         *)
(*   "pass"    that count, also bumped at the top of each pass of the      *)
(*             scheduler's wait loop (the current version)                 *)
(*                                                                         *)
(* In every mode a thread whose sleep_check_state reads sleeping is seen.  *)
(* For each thread the watchdog reads the count, then the sleep state, as  *)
(* two separate steps that threads can interleave with.                    *)
(*                                                                         *)
(* A thread outside its task runs the scheduler's wait loop as separate    *)
(* steps: bump the count ("loop"), then find a task, loop again if it was  *)
(* woken, or publish sleeping and park ("check"). A waker clears sleeping  *)
(* and unparks it, and the thread runs again some time later. Taking the   *)
(* task back is a switch, which also bumps the count.                      *)
(*                                                                         *)
(* Each thread is given one kind of task, chosen by the model checker:     *)
(*                                                                         *)
(*   "busy"    computes for ever and never yields                          *)
(*   "yield"   polls with `yield()` with nothing else to run, waking the   *)
(*             idle threads of its pool each time                          *)
(*   "switch"  two tasks that yield to each other                          *)
(*   "wait"    waits for an event (sleep, I/O) that some time later wakes  *)
(*             it                                                          *)
(*   "idle"    no task                                                     *)
(*                                                                         *)
(* A task other than "busy" ends after `Work` steps. The main task runs on *)
(* the first thread; once it ends, the ordinary exit runs, even if another *)
(* thread is still busy.                                                   *)
(*                                                                         *)
(* Time is counted in polls. A thread that has a step to take takes it     *)
(* within `Lag` polls; a parked thread has none. This is an assumption     *)
(* about the threads, and the model shows what it costs: with `Lag` too    *)
(* close to `Grace`, even the current version cuts off threads that do    *)
(* wait. Otherwise the checker picks the worst interleaving, so a sample   *)
(* that only sometimes misses a step counts as missing it.                 *)
(*                                                                         *)
(* When the fallback claims the exit, a Base listener task in the default  *)
(* pool becomes runnable. With `HookWait`, its atexit hook waits for that  *)
(* task, which only a free default-pool thread can run.                    *)
(*                                                                         *)
(* Left out: weak memory and counter wraparound, task migration, waits     *)
(* that the scheduler does not see (blocking foreign calls, runtime locks, *)
(* GC safepoints), hooks finishing in the ordinary exit, and what the      *)
(* fallback teardown does to the threads that keep running.                *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
    N,              \* number of pool threads, numbered 1..N; 1 runs the main task
    Interactive,    \* the threads in the interactive pool; the others are in the default pool
    Work,           \* steps a task takes before it ends
    Lag,            \* polls a thread may go without taking a step it has
    Grace,          \* polls a thread may go without being seen to wait
    Teardown,       \* polls the fallback teardown may take
    Watch,          \* "task", "switch" or "pass"
    HookWait        \* a hook waits for its listener in the fallback teardown

Threads == 1..N
Default == Threads \ Interactive
Kinds == {"busy", "yield", "switch", "wait", "idle"}

ASSUME /\ N \in Nat /\ N >= 1
       /\ Interactive \subseteq Threads
       /\ Work \in Nat /\ Lag \in Nat /\ Grace \in Nat /\ Teardown \in Nat
       /\ Watch \in {"task", "switch", "pass"}
       /\ HookWait \in BOOLEAN

\* whether a bump at this site moves the count in this mode
Counts(site) == Watch = "pass" \/ (Watch = "switch" /\ site = "switch")

VARIABLES
    kind,       \* kind[t]: the task thread t was given
    pc,         \* pc[t] in {"run", "loop", "check", "parked"}
    hastask,    \* hastask[t]: thread t's task has not ended
    work,       \* work[t]: steps left in thread t's task
    ready,      \* ready[t]: the event a "wait" task waits for has happened
    woken,      \* woken[t]: thread t was woken while it was not parked
    cur,        \* cur[t]: the task thread t runs ("a", "b" or "sched")
    sleeping,   \* sleeping[t]: thread t's sleep_check_state
    moved,      \* moved[t]: the count moved since the watchdog last read it
    lag,        \* lag[t]: polls since thread t last stepped
    wi,         \* the thread the watchdog reads next (N + 1: the poll is done)
    wread,      \* "count" or "sleep": which of thread wi's fields comes next
    same,       \* same[t]: the count (or task) read unchanged in this poll
    lastcur,    \* lastcur[t]: cur[t] at the watchdog's last read
    age,        \* age[t]: polls since thread t was last seen to wait
    claim,      \* "none", "ordinary" or "fallback": who runs the exit
    td,         \* fallback teardown: "none", "hooks", "done" or "killed"
    ticks       \* polls since the fallback claimed the exit

vars == <<kind, pc, hastask, work, ready, woken, cur, sleeping, moved, lag,
          wi, wread, same, lastcur, age, claim, td, ticks>>

Exited == claim = "ordinary" \/ td \in {"done", "killed"}
Listener == td = "hooks" /\ HookWait

TypeOK ==
    /\ kind \in [Threads -> Kinds]
    /\ pc \in [Threads -> {"run", "loop", "check", "parked"}]
    /\ hastask \in [Threads -> BOOLEAN]
    /\ work \in [Threads -> 0..Work]
    /\ ready \in [Threads -> BOOLEAN]
    /\ woken \in [Threads -> BOOLEAN]
    /\ cur \in [Threads -> {"a", "b", "sched"}]
    /\ sleeping \in [Threads -> BOOLEAN]
    /\ moved \in [Threads -> BOOLEAN]
    /\ lag \in [Threads -> 0..Lag]
    /\ wi \in 1..(N + 1)
    /\ wread \in {"count", "sleep"}
    /\ same \in [Threads -> BOOLEAN]
    /\ lastcur \in [Threads -> {"a", "b", "sched"}]
    /\ age \in [Threads -> 0..Grace]
    /\ claim \in {"none", "ordinary", "fallback"}
    /\ td \in {"none", "hooks", "done", "killed"}
    /\ ticks \in 0..Teardown

Init ==
    /\ kind \in [Threads -> Kinds]
    /\ pc = [t \in Threads |-> IF kind[t] = "idle" THEN "loop" ELSE "run"]
    /\ hastask = [t \in Threads |-> kind[t] # "idle"]
    /\ work = [t \in Threads |-> Work]
    /\ ready = [t \in Threads |-> FALSE]
    /\ woken = [t \in Threads |-> FALSE]
    /\ cur = [t \in Threads |-> IF kind[t] = "idle" THEN "sched" ELSE "a"]
    /\ sleeping = [t \in Threads |-> FALSE]
    /\ moved = [t \in Threads |-> FALSE]
    /\ lag = [t \in Threads |-> 0]
    /\ wi = 1
    /\ wread = "count"
    /\ same = [t \in Threads |-> FALSE]
    /\ lastcur = cur
    /\ age = [t \in Threads |-> 0]
    /\ claim = "none"
    /\ td = "none"
    /\ ticks = 0

----------------------------------------------------------------------------
(* Waking a thread: a parked one clears sleeping and goes back to the top  *)
(* of the loop when it next runs; any other one notes the wakeup.          *)

WakeAll(S) ==
    /\ sleeping' = [u \in Threads |-> IF u \in S /\ pc[u] = "parked" THEN FALSE ELSE sleeping[u]]
    /\ pc' = [u \in Threads |-> IF u \in S /\ pc[u] = "parked" THEN "loop" ELSE pc[u]]
    /\ woken' = [u \in Threads |-> IF u \in S /\ pc[u] # "parked" THEN TRUE ELSE woken[u]]

Idle == {u \in Threads : ~hastask[u]}
SamePool(t) == IF t \in Interactive THEN Interactive ELSE Default

(* A step by thread t: it resets the thread's lag and may bump its count.  *)
Stepped(t, site) ==
    /\ lag' = [lag EXCEPT ![t] = 0]
    /\ moved' = IF site # "none" /\ Counts(site) THEN [moved EXCEPT ![t] = TRUE] ELSE moved

Unchanged_W == UNCHANGED <<wi, wread, same, lastcur, age, claim, td, ticks>>

(* `yield()` with nothing else runnable switches to the running task.      *)
YieldStep(t) ==
    /\ ~Exited /\ pc[t] = "run" /\ kind[t] = "yield" /\ work[t] > 0
    /\ work' = [work EXCEPT ![t] = @ - 1]
    /\ Stepped(t, "switch")
    /\ WakeAll(Idle \cap SamePool(t))
    /\ UNCHANGED <<kind, hastask, ready, cur>> /\ Unchanged_W

SwitchStep(t) ==
    /\ ~Exited /\ pc[t] = "run" /\ kind[t] = "switch" /\ work[t] > 0
    /\ work' = [work EXCEPT ![t] = @ - 1]
    /\ cur' = [cur EXCEPT ![t] = IF @ = "a" THEN "b" ELSE "a"]
    /\ Stepped(t, "switch")
    /\ UNCHANGED <<kind, pc, hastask, ready, woken, sleeping>> /\ Unchanged_W

(* A wait runs the scheduler loop in the waiting task's context, so the    *)
(* current task does not change.                                           *)
WaitStart(t) ==
    /\ ~Exited /\ pc[t] = "run" /\ kind[t] = "wait" /\ work[t] > 0
    /\ pc' = [pc EXCEPT ![t] = "loop"]
    /\ ready' = [ready EXCEPT ![t] = FALSE]
    /\ Stepped(t, "none")
    /\ UNCHANGED <<kind, hastask, work, woken, cur, sleeping>> /\ Unchanged_W

(* The event a waiting task waits for happens, and wakes its thread.       *)
WaitEvent(t) ==
    /\ ~Exited /\ kind[t] = "wait" /\ hastask[t] /\ pc[t] # "run" /\ ~ready[t]
    /\ ready' = [ready EXCEPT ![t] = TRUE]
    /\ WakeAll({t})
    /\ UNCHANGED <<kind, hastask, work, cur, moved, lag>> /\ Unchanged_W

Finish(t) ==
    /\ ~Exited /\ pc[t] = "run" /\ kind[t] # "busy" /\ hastask[t] /\ work[t] = 0
    /\ hastask' = [hastask EXCEPT ![t] = FALSE]
    /\ cur' = [cur EXCEPT ![t] = "sched"]
    /\ pc' = [pc EXCEPT ![t] = "loop"]
    /\ Stepped(t, "switch")
    /\ UNCHANGED <<kind, work, ready, woken, sleeping>> /\ Unchanged_W

LoopTop(t) ==
    /\ ~Exited /\ pc[t] = "loop"
    /\ pc' = [pc EXCEPT ![t] = "check"]
    /\ Stepped(t, "pass")
    /\ UNCHANGED <<kind, hastask, work, ready, woken, cur, sleeping>> /\ Unchanged_W

(* The waiting task is runnable again: switch back to it.                  *)
Resume(t) ==
    /\ ~Exited /\ pc[t] = "check" /\ hastask[t] /\ ready[t]
    /\ pc' = [pc EXCEPT ![t] = "run"]
    /\ work' = [work EXCEPT ![t] = @ - 1]
    /\ woken' = [woken EXCEPT ![t] = FALSE]
    /\ Stepped(t, "switch")
    /\ UNCHANGED <<kind, hastask, ready, cur, sleeping>> /\ Unchanged_W

(* A free default-pool thread runs the listener task in the fallback.      *)
ListenerRuns(t) ==
    /\ pc[t] = "check" /\ ~hastask[t] /\ t \in Default /\ Listener
    /\ td' = "done"
    /\ Stepped(t, "switch")
    /\ UNCHANGED <<kind, pc, hastask, work, ready, woken, cur, sleeping,
                   wi, wread, same, lastcur, age, claim, ticks>>

NothingToRun(t) == ~(hastask[t] /\ ready[t]) /\ ~(~hastask[t] /\ t \in Default /\ Listener)

LoopAgain(t) ==
    /\ ~Exited /\ pc[t] = "check" /\ NothingToRun(t) /\ woken[t]
    /\ pc' = [pc EXCEPT ![t] = "loop"]
    /\ woken' = [woken EXCEPT ![t] = FALSE]
    /\ Stepped(t, "none")
    /\ UNCHANGED <<kind, hastask, work, ready, cur, sleeping>> /\ Unchanged_W

Park(t) ==
    /\ ~Exited /\ pc[t] = "check" /\ NothingToRun(t) /\ ~woken[t]
    /\ pc' = [pc EXCEPT ![t] = "parked"]
    /\ sleeping' = [sleeping EXCEPT ![t] = TRUE]
    /\ Stepped(t, "none")
    /\ UNCHANGED <<kind, hastask, work, ready, woken, cur>> /\ Unchanged_W

ThreadStep(t) ==
    \/ YieldStep(t) \/ SwitchStep(t) \/ WaitStart(t) \/ Finish(t)
    \/ LoopTop(t) \/ Resume(t) \/ ListenerRuns(t) \/ LoopAgain(t) \/ Park(t)

(* A thread owes a step unless it is parked or busy in its task.           *)
Owes(t) == ~(pc[t] = "parked") /\ ~(pc[t] = "run" /\ kind[t] = "busy")

----------------------------------------------------------------------------
(* The watchdog. It reads each thread's count, then its sleep state.       *)

ReadCount ==
    /\ claim = "none" /\ wi <= N /\ wread = "count"
    /\ same' = [same EXCEPT ![wi] =
                  IF Watch = "task" THEN cur[wi] = lastcur[wi] ELSE ~moved[wi]]
    /\ moved' = [moved EXCEPT ![wi] = FALSE]
    /\ lastcur' = [lastcur EXCEPT ![wi] = cur[wi]]
    /\ wread' = "sleep"
    /\ UNCHANGED <<kind, pc, hastask, work, ready, woken, cur, sleeping, lag,
                   wi, age, claim, td, ticks>>

ReadSleep ==
    /\ claim = "none" /\ wi <= N /\ wread = "sleep"
    /\ age' = [age EXCEPT ![wi] =
                 IF sleeping[wi] \/ ~same[wi] THEN 0
                 ELSE IF @ < Grace THEN @ + 1 ELSE Grace]
    /\ wi' = wi + 1
    /\ wread' = "count"
    /\ UNCHANGED <<kind, pc, hastask, work, ready, woken, cur, sleeping, moved,
                   lag, same, lastcur, claim, td, ticks>>

(* Time passes: one poll interval. Every thread that owes a step has taken *)
(* one within `Lag` polls.                                                 *)
TimePasses ==
    /\ \A t \in Threads : Owes(t) => lag[t] < Lag
    /\ lag' = [t \in Threads |-> IF Owes(t) THEN lag[t] + 1 ELSE 0]

(* The end of a poll: fire if some thread reached the grace period.        *)
PollEnd ==
    /\ claim = "none" /\ wi = N + 1
    /\ TimePasses
    /\ wi' = 1
    /\ IF \E t \in Threads : age[t] = Grace
          THEN /\ claim' = "fallback"
               \* hooks that wait for nothing are taken to finish at once
               /\ td' = IF HookWait THEN "hooks" ELSE "done"
               \* the listener task becomes runnable and wakes the free threads
               /\ WakeAll(Idle \cap Default)
          ELSE UNCHANGED <<claim, td, pc, sleeping, woken>>
    /\ UNCHANGED <<kind, hastask, work, ready, cur, moved, wread, same, lastcur,
                   age, ticks>>

(* The main task ended: the ordinary exit claims the exit.                 *)
OrdinaryExit ==
    /\ claim = "none" /\ ~hastask[1]
    /\ claim' = "ordinary"
    /\ UNCHANGED <<kind, pc, hastask, work, ready, woken, cur, sleeping, moved,
                   lag, wi, wread, same, lastcur, age, td, ticks>>

----------------------------------------------------------------------------
(* The fallback teardown, with its deadline counted in polls.              *)

Tick ==
    /\ td = "hooks" /\ ticks < Teardown
    /\ TimePasses
    /\ ticks' = ticks + 1
    /\ UNCHANGED <<kind, pc, hastask, work, ready, woken, cur, sleeping, moved,
                   wi, wread, same, lastcur, age, claim, td>>

Kill ==
    /\ td = "hooks" /\ ticks = Teardown
    /\ td' = "killed"
    /\ UNCHANGED <<kind, pc, hastask, work, ready, woken, cur, sleeping, moved,
                   lag, wi, wread, same, lastcur, age, claim, ticks>>

Next ==
    \/ \E t \in Threads : ThreadStep(t) \/ WaitEvent(t)
    \/ ReadCount \/ ReadSleep \/ PollEnd \/ OrdinaryExit
    \/ Tick \/ Kill
    \/ (Exited /\ UNCHANGED vars)

Spec ==
    /\ Init /\ [][Next]_vars
    /\ \A t \in Threads : WF_vars(ThreadStep(t)) /\ WF_vars(WaitEvent(t))
    /\ WF_vars(ReadCount) /\ WF_vars(ReadSleep) /\ WF_vars(PollEnd)
    /\ WF_vars(OrdinaryExit) /\ WF_vars(Tick) /\ WF_vars(Kill)

----------------------------------------------------------------------------
(* Properties. *)

(* The fallback runs only when some thread really is busy.                 *)
NoFalseFire == claim = "fallback" => \E t \in Threads : kind[t] = "busy"

(* The fallback teardown finishes before its deadline.                     *)
NoDeadlineKill == td # "killed"

(* Some exit always starts, and without busy code it is the ordinary one.  *)
Exits == <>Exited
CooperatingExitsOrdinarily ==
    (\A t \in Threads : kind[t] # "busy") => <>(claim = "ordinary")

=============================================================================
