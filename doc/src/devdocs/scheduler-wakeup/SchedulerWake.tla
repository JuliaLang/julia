-------------------------- MODULE SchedulerWake --------------------------
(***************************************************************************)
(* Julia's task scheduler sleep/wake handshake: the wake-one strategy of     *)
(* `jl_wakeup_threadpool` (JuliaLang/julia#61826) plus the searcher          *)
(* accounting and count-gated wakeups of JuliaLang/julia#62284.              *)
(*                                                                           *)
(* Every action is atomic; TLC explores all sequentially-consistent          *)
(* interleavings. The C code realizes this ordering with the store-buffering *)
(* fences documented in src/scheduler.c ([^store_buffering_1]).              *)
(*                                                                           *)
(* Workers consume tasks only from their own pool's queue. A worker that     *)
(* finds no work may become a *searcher* (polling the queues) while fewer    *)
(* than half of its pool holds a searcher slot; otherwise it parks without   *)
(* polling. The sleep transition follows `jl_task_get_next`:                 *)
(*                                                                           *)
(*   RELEASE  searchers: drop the slot          ("run" -> "exitspin")        *)
(*   PUBLISH  sleep_check_state := "sleeping"   (pc -> "recheck")            *)
(*   RECHECK  re-check own queue; abort the sleep if non-empty               *)
(*   RETIRE   decrement n_threads_running       (pc -> "park")               *)
(*   PARK     commit iff still "sleeping"; else a waker raced us             *)
(*                                                                           *)
(* An enqueue wakes at most one worker, gated on the count of committed      *)
(* searchers vs pending tasks, and a wake starts the woken worker as a       *)
(* searcher (in-flight wakes count as supply at the gate). The last          *)
(* searcher to stop looking is responsible for waking a successor if work    *)
(* remains (the exit handoff), on both the found-work and unwind exits.      *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
    Threads,        \* set of worker ids, e.g. {1, 2}
    Pool,           \* function Threads -> pool id, e.g. (1 :> "A" @@ 2 :> "B")
    Inject0,        \* function pool -> Nat: tasks producers will inject per pool
    UnwindHandsOff   \* BOOLEAN: does a last-searcher unwind wake a successor?
                     \* TRUE matches the implementation; FALSE omits the wake
                     \* and loses the wakeup deferred onto the spinner.

VARIABLES
    st,             \* st[t] in {"running", "sleeping"} -- the sleep_check_state
    pc,             \* pc[t] in {"run", "exitspin", "handoff", "unwindwake",
                    \*           "recheck", "park", "parked", "outside"}
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

(* Parked, or unwound out of the scheduler: an unwinding thread is awake but  *)
(* never consults the queues again, so it counts the same as a parked one.    *)
Blocked(t)   == pc[t] \in {"parked", "outside"}

QueueEmpty   == \A p \in Pools : queue[p] = 0

(* Every worker of a pool unwound. "outside" is permanent in the model, but   *)
(* an unwinding worker always re-enters jl_task_get_next through its task's   *)
(* teardown, so this state heals by re-entry; NoLostWakeup excuses it.        *)
AllOutside(p) == \A t \in ThreadsOf(p) : pc[t] = "outside"

TypeOK ==
    /\ st    \in [Threads -> {"running", "sleeping"}]
    /\ pc    \in [Threads -> {"run", "exitspin", "handoff", "unwindwake",
                             "recheck", "park", "parked", "outside"}]
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

(* Wake one sleeping worker: flip it to running, bump the running count,     *)
(* release it if parked, and start it as a searcher. The slot increment and   *)
(* the CAS are one action here because the C increments before the CAS and    *)
(* undoes it on failure; a sleeping worker holds no slot, so the increment     *)
(* cannot double-count.                                                        *)
WakeOne(p) ==
    \E t \in ThreadsOf(p) :
        /\ st[t] = "sleeping"
        /\ st'   = [st EXCEPT ![t] = "running"]
        /\ nrun' = nrun + 1
        /\ pc'   = [pc EXCEPT ![t] = IF pc[t] = "parked" THEN "run" ELSE @]
        /\ spin' = [spin EXCEPT ![t] = TRUE]
        /\ nspin' = [nspin EXCEPT ![p] = @ + 1]

(* The count gate: wake nobody while committed searchers cover the pending    *)
(* tasks; otherwise wake at most one sleeper.                                 *)
Wakeup(p, pending) ==
    IF nspin[p] >= pending \/ ~CanWakeIn(p)
        THEN UNCHANGED <<st, pc, nrun, spin, nspin>>
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
            /\ Wakeup(p, queue[p] + 1)

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

(* A searcher pops a task: release the slot; the pool's last searcher owes    *)
(* the exit handoff.                                                          *)
ConsumeSpinner ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ spin[t]
        /\ queue[Pool[t]] > 0
        /\ queue' = [queue EXCEPT ![Pool[t]] = @ - 1]
        /\ spin'  = [spin EXCEPT ![t] = FALSE]
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ - 1]
        /\ pc'    = [pc EXCEPT ![t] = IF nspin[Pool[t]] = 1 THEN "handoff" ELSE @]
        /\ UNCHANGED <<st, nrun, inject>>

(* The handoff wake is its own step (a separate jl_wakeup_threadpool call in  *)
(* the C code), so other threads may interleave before it runs.               *)
HandoffWake ==
    \E t \in Threads :
        /\ pc[t] = "handoff"
        /\ LET p == Pool[t] IN
           IF nspin[p] >= queue[p] \/ ~CanWakeIn(p)
               THEN /\ pc' = [pc EXCEPT ![t] = "run"]
                    /\ UNCHANGED <<st, nrun, spin, nspin>>
               ELSE \E w \in ThreadsOf(p) :
                       /\ st[w] = "sleeping"
                       /\ st'   = [st EXCEPT ![w] = "running"]
                       /\ nrun' = nrun + 1
                       /\ spin'  = [spin EXCEPT ![w] = TRUE]
                       /\ nspin' = [nspin EXCEPT ![p] = @ + 1]
                       /\ pc'   = [pc EXCEPT ![t] = "run",
                                              ![w] = IF pc[w] = "parked" THEN "run" ELSE pc[w]]
        /\ UNCHANGED <<queue, inject>>

(* RELEASE: drop the slot before PUBLISH. An enqueuer that saw the slot is    *)
(* ordered before this step, so its task lands before our RECHECK.            *)
SpinExit ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ spin[t]
        /\ spin'  = [spin EXCEPT ![t] = FALSE]
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ - 1]
        /\ pc'    = [pc EXCEPT ![t] = "exitspin"]
        /\ UNCHANGED <<st, nrun, queue, inject>>

(* An exception unwinds a searcher out of the scheduler (trypoptask throwing, *)
(* or SIGINT surfacing there -- a routine exit: every interactive Ctrl-C      *)
(* takes it). Unlike SpinExit there is no post-publish        *)
(* recheck, so the pool's last searcher owes the same exit handoff             *)
(* (UnwindHandsOff = FALSE omits it and loses the deferred wakeup).            *)
ThrowSpinner ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ spin[t]
        /\ spin'  = [spin EXCEPT ![t] = FALSE]
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ - 1]
        /\ pc'    = [pc EXCEPT ![t] =
                        IF nspin[Pool[t]] = 1 /\ UnwindHandsOff
                            THEN "unwindwake" ELSE "outside"]
        /\ UNCHANGED <<st, nrun, queue, inject>>

(* The unwind-path handoff (the wake in the outer JL_CATCH).                  *)
UnwindHandoff ==
    \E t \in Threads :
        /\ pc[t] = "unwindwake"
        /\ LET p == Pool[t] IN
           IF nspin[p] >= queue[p] \/ ~CanWakeIn(p)
               THEN /\ pc' = [pc EXCEPT ![t] = "outside"]
                    /\ UNCHANGED <<st, nrun, spin, nspin>>
               ELSE \E w \in ThreadsOf(p) :
                       /\ st[w] = "sleeping"
                       /\ st'   = [st EXCEPT ![w] = "running"]
                       /\ nrun' = nrun + 1
                       /\ spin'  = [spin EXCEPT ![w] = TRUE]
                       /\ nspin' = [nspin EXCEPT ![p] = @ + 1]
                       /\ pc'   = [pc EXCEPT ![t] = "outside",
                                              ![w] = IF pc[w] = "parked" THEN "run" ELSE pc[w]]
        /\ UNCHANGED <<queue, inject>>

(* PUBLISH: non-searchers enter from "run" (a worker denied a slot parks      *)
(* without polling); ex-searchers from "exitspin".                            *)
SleepBegin ==
    \E t \in Threads :
        /\ pc[t] \in {"run", "exitspin"}
        /\ st[t] = "running"
        /\ ~spin[t]
        /\ st' = [st EXCEPT ![t] = "sleeping"]
        /\ pc' = [pc EXCEPT ![t] = "recheck"]
        /\ UNCHANGED <<spin, nspin, nrun, queue, inject>>

(* RECHECK: abort the sleep if work appeared. Mirrors set_not_sleeping: a    *)
(* self-flip leaves the counter alone; a raced flip consumes the waker's      *)
(* in-flight increment.                                                       *)
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

(* RETIRE + PARK: leave the running count, then commit unless a waker won    *)
(* the race (its nrun++ balances our decrement).                              *)
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
    \/ HandoffWake
    \/ SpinExit
    \/ ThrowSpinner
    \/ UnwindHandoff
    \/ SleepBegin
    \/ SleepRecheckAbortSelf
    \/ SleepRecheckAbortRaced
    \/ SleepRecheckEmpty
    \/ ParkCommit
    \/ ParkRaced
    \* Quiescence with empty queues is legitimate (sleeping producers never
    \* inject their remaining tasks). Allowing a stutter step there makes
    \* TLC's deadlock detector fire only when work is stuck in a queue.
    \/ ((\A p \in Pools : queue[p] = 0 \/ AllOutside(p)) /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

----------------------------------------------------------------------------
(* Properties. *)

(* Everything that was queued has been consumed. *)
Done == QueueEmpty /\ \A p \in Pools : inject[p] = 0

(* A non-empty queue always has an unblocked worker in its pool, except the    *)
(* AllOutside case that re-entry heals. Produce enqueues and wakes atomically,  *)
(* so this cannot fail transiently -- a violation is a genuine lost wakeup.     *)
NoLostWakeup ==
    \A p \in Pools :
        (queue[p] > 0) =>
            \/ \E t \in ThreadsOf(p) : ~Blocked(t)
            \/ AllOutside(p)

=============================================================================
