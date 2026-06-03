-------------------------- MODULE SchedulerWake --------------------------
(***************************************************************************)
(* A TLA+ model of Julia's task scheduler sleep/wake handshake, focused on *)
(* the `jl_wakeup_threadpool` "wake one worker" strategy introduced in      *)
(* JuliaLang/julia#61826 and the soundness of skipping the wake based on     *)
(* `n_threads_running`.                                                      *)
(*                                                                           *)
(* It abstracts away weak memory: every action below is atomic and the       *)
(* model checker explores all sequentially-consistent interleavings. That is *)
(* sufficient to expose the *algorithmic* race the review worried about,     *)
(* because the bug is about which shared variable the waker consults         *)
(* (`n_threads_running` vs. each worker's `sleep_check_state`) and the order  *)
(* in which a consumer re-checks its queue versus decrementing the running   *)
(* count -- not about fences.                                                *)
(*                                                                           *)
(* Each worker belongs to a thread pool. A worker only consumes tasks from   *)
(* its own pool's queue. The sleep transition is split into the same steps   *)
(* as `jl_task_get_next`:                                                     *)
(*                                                                           *)
(*   c1  set sleep_check_state := "sleeping"   (pc: "run" -> "recheck")       *)
(*   c2  re-check own queue; if non-empty, abort the sleep (back to "run")    *)
(*   c3  pre-park: decrement n_threads_running  (pc: "recheck" -> "park")     *)
(*   c4  commit: if still "sleeping", actually park; else a waker raced us    *)
(*                                                                           *)
(* The crucial window is [c1 .. c3): the worker has published                *)
(* `sleep_check_state = sleeping` but is *still counted* in n_threads_running *)
(* and may have already read its queue as empty at c2. A waker that consults  *)
(* the count therefore sees stale "all running" information; a waker that     *)
(* scans `sleep_check_state` sees the truth.                                  *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
    Threads,        \* set of worker ids, e.g. {1, 2}
    Pool,           \* function Threads -> pool id, e.g. (1 :> "A" @@ 2 :> "B")
    Inject0,        \* function pool -> Nat: tasks producers will inject per pool
    EarlyOut        \* TRUE  -> buggy: skip the scan when n_threads_running is "full"
                    \* FALSE -> fixed: always scan sleep_check_state

VARIABLES
    st,             \* st[t] in {"running", "sleeping"} -- the sleep_check_state
    pc,             \* pc[t] in {"run", "recheck", "park", "parked"}
    nrun,           \* n_threads_running counter
    queue,          \* queue[p] in Nat: enqueued-but-unconsumed tasks in pool p
    inject          \* inject[p] in Nat: tasks still to be produced into pool p

vars == <<st, pc, nrun, queue, inject>>

Pools     == { Pool[t] : t \in Threads }
ThreadsOf(p) == { t \in Threads : Pool[t] = p }
N         == Cardinality(Threads)

RECURSIVE SumOver(_, _)
SumOver(acc, S) == IF S = {} THEN acc
                   ELSE LET x == CHOOSE y \in S : TRUE
                        IN  SumOver(acc + Inject0[x], S \ {x})

Inject0Total == SumOver(0, Pools)

(* A worker is "blocked" once it has committed to the sleep (pc = "parked"). *)
Blocked(t)   == pc[t] = "parked"

QueueEmpty   == \A p \in Pools : queue[p] = 0

TypeOK ==
    /\ st    \in [Threads -> {"running", "sleeping"}]
    /\ pc    \in [Threads -> {"run", "recheck", "park", "parked"}]
    /\ nrun  \in 0..(2 * N)
    /\ queue \in [Pools -> 0..Inject0Total]
    /\ inject \in [Pools -> 0..Inject0Total]

Init ==
    /\ st    = [t \in Threads |-> "running"]
    /\ pc    = [t \in Threads |-> "run"]
    /\ nrun  = N
    /\ queue = [p \in Pools |-> 0]
    /\ inject = [p \in Pools |-> Inject0[p]]

----------------------------------------------------------------------------
(* The wake step shared by both variants: wake (at most) one worker in pool  *)
(* `p` whose sleep_check_state is "sleeping".                                *)

CanWakeIn(p) == \E t \in ThreadsOf(p) : st[t] = "sleeping"

(* Wake exactly one sleeping worker in p: flip it back to running, bump the   *)
(* running count, and (if it had already parked) release it.                  *)
WakeOne(p) ==
    \E t \in ThreadsOf(p) :
        /\ st[t] = "sleeping"
        /\ st'   = [st EXCEPT ![t] = "running"]
        /\ nrun' = nrun + 1
        /\ pc'   = [pc EXCEPT ![t] = IF pc[t] = "parked" THEN "run" ELSE @]

(* Apply the wakeup policy for pool p after an insert. *)
Wakeup(p) ==
    IF EarlyOut /\ nrun >= N
        THEN /\ UNCHANGED <<st, pc, nrun>>      \* buggy short-circuit
        ELSE IF CanWakeIn(p)
            THEN WakeOne(p)
            ELSE UNCHANGED <<st, pc, nrun>>      \* nobody parked: nothing to do

----------------------------------------------------------------------------
(* Producer: a running worker injects one pending task into some pool's queue *)
(* and runs the wakeup policy. Models `@spawn` (possibly cross-pool).         *)
Produce ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ \E p \in Pools :
            /\ inject[p] > 0
            /\ inject' = [inject EXCEPT ![p] = @ - 1]
            /\ queue'  = [queue EXCEPT ![p] = @ + 1]
            /\ Wakeup(p)

(* Consumer fast path: a running worker pops a task from its own queue. *)
Consume ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ queue[Pool[t]] > 0
        /\ queue' = [queue EXCEPT ![Pool[t]] = @ - 1]
        /\ UNCHANGED <<st, pc, nrun, inject>>

(* c1: begin the sleep transition by publishing sleep_check_state = sleeping. *)
SleepBegin ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ st' = [st EXCEPT ![t] = "sleeping"]
        /\ pc' = [pc EXCEPT ![t] = "recheck"]
        /\ UNCHANGED <<nrun, queue, inject>>

(* c2: re-check the queue. If work appeared, abort the sleep (still counted). *)
SleepRecheckAbort ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ queue[Pool[t]] > 0
        /\ st' = [st EXCEPT ![t] = "running"]
        /\ pc' = [pc EXCEPT ![t] = "run"]
        /\ UNCHANGED <<nrun, queue, inject>>

SleepRecheckEmpty ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ queue[Pool[t]] = 0
        /\ pc' = [pc EXCEPT ![t] = "park"]
        /\ UNCHANGED <<st, nrun, queue, inject>>

(* c3 + c4: decrement the running count, then commit to park only if no waker *)
(* flipped us back to running in the meantime. (nrun-- happens before the     *)
(* park; a racing waker's nrun++ balances it.)                                *)
ParkCommit ==
    \E t \in Threads :
        /\ pc[t] = "park"
        /\ st[t] = "sleeping"
        /\ nrun' = nrun - 1
        /\ pc'   = [pc EXCEPT ![t] = "parked"]
        /\ UNCHANGED <<st, queue, inject>>

ParkRaced ==
    \E t \in Threads :
        /\ pc[t] = "park"
        /\ st[t] = "running"        \* a waker won the race during the window
        /\ nrun' = nrun - 1         \* consume our own pre-park decrement...
        /\ pc'   = [pc EXCEPT ![t] = "run"]
        /\ UNCHANGED <<st, queue, inject>>

Next ==
    \/ Produce
    \/ Consume
    \/ SleepBegin
    \/ SleepRecheckAbort
    \/ SleepRecheckEmpty
    \/ ParkCommit
    \/ ParkRaced
    \* When no task is queued the system is legitimately quiescent (producers
    \* that went to sleep simply never inject their remaining tasks). Allow a
    \* stutter step there so TLC's deadlock detector only fires on states that
    \* are stuck with work *still in a queue* -- i.e. a genuine lost wakeup.
    \/ (QueueEmpty /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

----------------------------------------------------------------------------
(* Properties. *)

(* Everything that was queued has been consumed. *)
Done == QueueEmpty /\ \A p \in Pools : inject[p] = 0

(* Safety: a non-empty queue must always have a worker in its pool that is    *)
(* not blocked (so it will eventually re-check and consume). This is the       *)
(* "no permanently lost wakeup" invariant. Because Produce enqueues and runs   *)
(* the wakeup policy atomically here, it can only fail in a genuine stuck       *)
(* state, never transiently.                                                   *)
NoLostWakeup ==
    \A p \in Pools :
        (queue[p] > 0) =>
            \E t \in ThreadsOf(p) : ~Blocked(t)

=============================================================================
