-------------------------- MODULE SchedulerWake --------------------------
(***************************************************************************)
(* A TLA+ model of Julia's task scheduler sleep/wake handshake: the         *)
(* `jl_wakeup_threadpool` "wake one worker" strategy (JuliaLang/julia#61826) *)
(* plus the spinner accounting added in JuliaLang/julia#62284.               *)
(*                                                                           *)
(* It abstracts away weak memory: every action below is atomic and the       *)
(* model checker explores all sequentially-consistent interleavings. That is *)
(* sufficient to check the *algorithmic* claim, because correctness rests on  *)
(* which shared variables the waker consults (each worker's                  *)
(* sleep_check_state and the pool's spinner count) and the order in which a  *)
(* consumer publishes those versus re-checking its queue -- not on fences.    *)
(*                                                                           *)
(* Each worker belongs to a thread pool. A worker only consumes tasks from   *)
(* its own pool's queue. A worker that finds no work may become a *spinner*   *)
(* (busy-polling the queues) if fewer than half of its pool holds a spinner   *)
(* slot; otherwise it parks without polling. The sleep transition is split    *)
(* into the same steps as `jl_task_get_next`:                                 *)
(*                                                                           *)
(*   s0  spinners only: release the spinner slot   (pc: "run" -> "exitspin") *)
(*   c1  set sleep_check_state := "sleeping"   (pc: -> "recheck")             *)
(*   c2  re-check own queue; if non-empty, abort the sleep (back to "run")    *)
(*   c3  pre-park: decrement n_threads_running  (pc: "recheck" -> "park")     *)
(*   c4  commit: if still "sleeping", actually park; else a waker raced us    *)
(*                                                                           *)
(* The waker consults n_spinning first: while a spinner exists in the target  *)
(* pool it wakes nobody -- the spinner will find the work. This is safe       *)
(* because a spinner releases its slot at s0, *before* the c1 publish and     *)
(* the c2 re-check: an enqueuer that still observes the slot is ordered       *)
(* before s0, hence before c2, so the spinner's re-check sees its task.       *)
(* (In the C code this pairing is the store-buffering fence dance             *)
(* [^store_buffering_1]; here sequential consistency plays that role.)        *)
(*                                                                           *)
(* If no spinner exists, the waker scans sleep_check_state, which a worker    *)
(* publishes at c1 (before the c2 re-check), so the scan never misses a       *)
(* worker in the [c1 .. c3) window even though that worker is still counted   *)
(* in n_threads_running.                                                      *)
(*                                                                           *)
(* A spinner that *finds work* releases its slot and, if it was the pool's    *)
(* last spinner, runs the wakeup policy once ("wake propagation"). That is a  *)
(* parallelism property, not a safety one -- the worker holding the task is   *)
(* itself awake -- but it is modeled because the implementation does it and   *)
(* it closes the window where a burst absorbed by one spinner would leave     *)
(* the rest of the pool parked until that worker blocks.                      *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
    Threads,        \* set of worker ids, e.g. {1, 2}
    Pool,           \* function Threads -> pool id, e.g. (1 :> "A" @@ 2 :> "B")
    Inject0         \* function pool -> Nat: tasks producers will inject per pool

VARIABLES
    st,             \* st[t] in {"running", "sleeping"} -- the sleep_check_state
    pc,             \* pc[t] in {"run", "exitspin", "propagate", "recheck", "park", "parked"}
    spin,           \* spin[t] in BOOLEAN -- does t hold a spinner slot?
    nspin,          \* nspin[p] -- the pool's n_spinning counter
    nrun,           \* n_threads_running counter
    queue,          \* queue[p] in Nat: enqueued-but-unconsumed tasks in pool p
    inject          \* inject[p] in Nat: tasks still to be produced into pool p

vars == <<st, pc, spin, nspin, nrun, queue, inject>>

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
    /\ pc    \in [Threads -> {"run", "exitspin", "propagate", "recheck", "park", "parked"}]
    /\ spin  \in [Threads -> BOOLEAN]
    /\ nspin \in [Pools -> 0..N]
    /\ nrun  \in 0..(2 * N)
    /\ queue \in [Pools -> 0..Inject0Total]
    /\ inject \in [Pools -> 0..Inject0Total]

(* The n_spinning counter always agrees with the slots actually held. *)
SpinCountOK ==
    \A p \in Pools : nspin[p] = Cardinality({ t \in ThreadsOf(p) : spin[t] })

Init ==
    /\ st    = [t \in Threads |-> "running"]
    /\ pc    = [t \in Threads |-> "run"]
    /\ spin  = [t \in Threads |-> FALSE]
    /\ nspin = [p \in Pools |-> 0]
    /\ nrun  = N
    /\ queue = [p \in Pools |-> 0]
    /\ inject = [p \in Pools |-> Inject0[p]]

----------------------------------------------------------------------------
(* Wake (at most) one worker in pool `p` whose sleep_check_state is sleeping. *)

CanWakeIn(p) == \E t \in ThreadsOf(p) : st[t] = "sleeping"

(* Wake exactly one sleeping worker in p: flip it back to running, bump the   *)
(* running count, and (if it had already parked) release it.                  *)
WakeOne(p) ==
    \E t \in ThreadsOf(p) :
        /\ st[t] = "sleeping"
        /\ st'   = [st EXCEPT ![t] = "running"]
        /\ nrun' = nrun + 1
        /\ pc'   = [pc EXCEPT ![t] = IF pc[t] = "parked" THEN "run" ELSE @]

(* Apply the wakeup policy for pool p after an insert. A spinner in the pool  *)
(* is searching the queues and will find the work: wake nobody. Otherwise     *)
(* scan sleep_check_state and wake at most one sleeper.                       *)
Wakeup(p) ==
    IF nspin[p] > 0 \/ ~CanWakeIn(p)
        THEN UNCHANGED <<st, pc, nrun>>
        ELSE WakeOne(p)

----------------------------------------------------------------------------
(* Producer: a running worker injects one pending task into some pool's queue *)
(* and runs the wakeup policy. Models `@spawn` (possibly cross-pool).         *)
Produce ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ ~spin[t]
        /\ \E p \in Pools :
            /\ inject[p] > 0
            /\ inject' = [inject EXCEPT ![p] = @ - 1]
            /\ queue'  = [queue EXCEPT ![p] = @ + 1]
            /\ Wakeup(p)
            /\ UNCHANGED <<spin, nspin>>

(* Consumer fast path: a running, non-spinning worker pops a task. *)
Consume ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ ~spin[t]
        /\ queue[Pool[t]] > 0
        /\ queue' = [queue EXCEPT ![Pool[t]] = @ - 1]
        /\ UNCHANGED <<st, pc, spin, nspin, nrun, inject>>

(* A worker that found no work takes a spinner slot, if under the pool's cap  *)
(* (at most half the pool, and at least one).                                 *)
SpinEnter ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ ~spin[t]
        /\ 2 * nspin[Pool[t]] < Cardinality(ThreadsOf(Pool[t]))
        /\ spin'  = [spin EXCEPT ![t] = TRUE]
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ + 1]
        /\ UNCHANGED <<st, pc, nrun, queue, inject>>

(* A spinner pops a task: release the slot, and if it was the pool's last     *)
(* spinner, remember to run the wakeup policy once (wake propagation).        *)
ConsumeSpinner ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ spin[t]
        /\ queue[Pool[t]] > 0
        /\ queue' = [queue EXCEPT ![Pool[t]] = @ - 1]
        /\ spin'  = [spin EXCEPT ![t] = FALSE]
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ - 1]
        /\ pc'    = [pc EXCEPT ![t] = IF nspin[Pool[t]] = 1 THEN "propagate" ELSE @]
        /\ UNCHANGED <<st, nrun, inject>>

(* The propagation wake runs as its own step (in the C code it is a separate  *)
(* jl_wakeup_threadpool call), so other threads may interleave. Spelled out    *)
(* rather than reusing Wakeup because both the propagator's and the woken      *)
(* worker's pc change in the same step.                                        *)
PropagateWake ==
    \E t \in Threads :
        /\ pc[t] = "propagate"
        /\ LET p == Pool[t] IN
           IF nspin[p] > 0 \/ ~CanWakeIn(p)
               THEN /\ pc' = [pc EXCEPT ![t] = "run"]
                    /\ UNCHANGED <<st, nrun>>
               ELSE \E w \in ThreadsOf(p) :
                       /\ st[w] = "sleeping"
                       /\ st'   = [st EXCEPT ![w] = "running"]
                       /\ nrun' = nrun + 1
                       /\ pc'   = [pc EXCEPT ![t] = "run",
                                              ![w] = IF pc[w] = "parked" THEN "run" ELSE pc[w]]
        /\ UNCHANGED <<spin, nspin, queue, inject>>

(* s0: a spinner heading to sleep releases its slot *first*, before the c1    *)
(* publish below. An enqueuer that saw the slot is ordered before this step   *)
(* and its task lands before our c2 re-check.                                 *)
SpinExit ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ spin[t]
        /\ spin'  = [spin EXCEPT ![t] = FALSE]
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ - 1]
        /\ pc'    = [pc EXCEPT ![t] = "exitspin"]
        /\ UNCHANGED <<st, nrun, queue, inject>>

(* c1: begin the sleep transition by publishing sleep_check_state = sleeping. *)
(* Non-spinners enter from "run" (a worker denied a spinner slot parks        *)
(* without polling); ex-spinners enter from "exitspin".                       *)
SleepBegin ==
    \E t \in Threads :
        /\ pc[t] \in {"run", "exitspin"}
        /\ st[t] = "running"
        /\ ~spin[t]
        /\ st' = [st EXCEPT ![t] = "sleeping"]
        /\ pc' = [pc EXCEPT ![t] = "recheck"]
        /\ UNCHANGED <<spin, nspin, nrun, queue, inject>>

(* c2: re-check the queue. If work appeared, abort the sleep. Mirrors        *)
(* set_not_sleeping: if we flip the state ourselves, the counter is untouched *)
(* (no waker incremented for us); if a waker raced us and already flipped it, *)
(* we consume its in-flight increment.                                        *)
SleepRecheckAbortSelf ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ st[t] = "sleeping"
        /\ queue[Pool[t]] > 0
        /\ st' = [st EXCEPT ![t] = "running"]
        /\ pc' = [pc EXCEPT ![t] = "run"]
        /\ UNCHANGED <<spin, nspin, nrun, queue, inject>>

SleepRecheckAbortRaced ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ st[t] = "running"          \* a waker flipped us and incremented
        /\ queue[Pool[t]] > 0
        /\ nrun' = nrun - 1           \* consume the in-flight wakeup
        /\ pc' = [pc EXCEPT ![t] = "run"]
        /\ UNCHANGED <<st, spin, nspin, queue, inject>>

SleepRecheckEmpty ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ queue[Pool[t]] = 0
        /\ pc' = [pc EXCEPT ![t] = "park"]
        /\ UNCHANGED <<st, spin, nspin, nrun, queue, inject>>

(* c3 + c4: decrement the running count, then commit to park only if no waker *)
(* flipped us back to running in the meantime. (nrun-- happens before the     *)
(* park; a racing waker's nrun++ balances it.)                                *)
ParkCommit ==
    \E t \in Threads :
        /\ pc[t] = "park"
        /\ st[t] = "sleeping"
        /\ nrun' = nrun - 1
        /\ pc'   = [pc EXCEPT ![t] = "parked"]
        /\ UNCHANGED <<st, spin, nspin, queue, inject>>

ParkRaced ==
    \E t \in Threads :
        /\ pc[t] = "park"
        /\ st[t] = "running"        \* a waker won the race during the window
        /\ nrun' = nrun - 1         \* consume our own pre-park decrement...
        /\ pc'   = [pc EXCEPT ![t] = "run"]
        /\ UNCHANGED <<st, spin, nspin, queue, inject>>

Next ==
    \/ Produce
    \/ Consume
    \/ SpinEnter
    \/ ConsumeSpinner
    \/ PropagateWake
    \/ SpinExit
    \/ SleepBegin
    \/ SleepRecheckAbortSelf
    \/ SleepRecheckAbortRaced
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
