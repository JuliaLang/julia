-------------------------- MODULE SchedulerWake --------------------------
(***************************************************************************)
(* Julia's task scheduler sleep/wake handshake: `jl_wakeup_threadpool`       *)
(* wakes at most one worker per insert, gated on searcher accounting.        *)
(*                                                                           *)
(* Every action is atomic; TLC explores all sequentially-consistent          *)
(* interleavings. The C code realizes this ordering with the store-buffering *)
(* fences documented in src/scheduler.c ([^store_buffering_1]).              *)
(*                                                                           *)
(* Workers consume tasks only from their own pool's queue. A worker that     *)
(* finds no work may become a *searcher* (polling the queues) while a slot   *)
(* is free, and otherwise parks without polling. The C limits searchers to   *)
(* half the pool, or all of it for some threads; the model admits up to the  *)
(* whole pool, which covers both. The sleep transition follows               *)
(* `jl_task_get_next`:                                                       *)
(*                                                                           *)
(*   RELEASE  searchers: drop the slot          ("run" -> "exitspin")        *)
(*   PUBLISH  sleep_check_state := "sleeping"   (pc -> "recheck")            *)
(*   RECHECK  re-check own queue; abort the sleep if non-empty               *)
(*   RETIRE   decrement n_threads_running       (pc -> "park")               *)
(*   PARK     commit iff still "sleeping"; else a waker raced us             *)
(*                                                                           *)
(* An enqueue wakes a sleeper only if nobody searches the pool, and a wake   *)
(* starts the woken worker as a searcher (in-flight wakes count at the       *)
(* gate). The last searcher to stop looking wakes a successor if nobody else *)
(* searches (the exit handoff): after finding work, only if the queue is     *)
(* still non-empty. A searcher that aborts its park because work appeared    *)
(* owes the handoff if its next pop finds work.                              *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
    Threads,        \* set of worker ids, e.g. {1, 2}
    Pool,           \* function Threads -> pool id, e.g. (1 :> "A" @@ 2 :> "B")
    Inject0,        \* function pool -> Nat: tasks producers will inject per pool
    UnwindHandsOff,  \* BOOLEAN: does a last-searcher unwind wake a successor?
                     \* TRUE matches the implementation; FALSE omits the wake
                     \* and loses the wakeup deferred onto the spinner.
    RollbackHandsOff, \* BOOLEAN: does a failed wake's last-slot rollback hand off?
    OwedUnwindHandsOff, \* BOOLEAN: does a worker owing a wake hand off if it throws?
    RefusedRechecks \* BOOLEAN: does a worker refused a slot recheck reliably?
                    \* FALSE lets its recheck miss queued work, like a lone pop.

VARIABLES
    st,             \* st[t] in {"running", "sleeping"} -- the sleep_check_state
    pc,             \* pc[t] in {"run", "exitspin", "counted", "handoff", "unwindwake",
                    \*           "recheck", "park", "parked", "outside"}
    spin,           \* spin[t] in BOOLEAN -- does t hold a spinner slot?
    nspin,          \* nspin[p] -- the pool's n_spinning counter
    nsleep,         \* nsleep[p] -- the pool's n_sleeping counter
    nrun,           \* n_threads_running counter
    queue,          \* queue[p] in Nat: enqueued-but-unconsumed tasks in pool p
    inject,         \* inject[p] in Nat: tasks still to be produced into pool p
    prewake,        \* targets of wake attempts paused before their sleep-state CAS
    handoffs,       \* pools owed a deferred wake after a last-slot rollback
    held            \* held[t]: t released a searcher slot entering its sleep transition,
                    \* and after aborting the park, that it owes the exit handoff

vars == <<st, pc, spin, nspin, nsleep, nrun, queue, inject, prewake, handoffs, held>>

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

(* Every worker of a pool unwound. An outside worker is running user code,    *)
(* not parked, so no wakeup can be lost on it; work queued while every        *)
(* worker is outside waits until one blocks, which re-enters the scheduler.   *)
(* The wake protocol's obligation covers only wakeable (parked) workers,      *)
(* which is NoLostWakeup's other disjunct; this case is excused.              *)
AllOutside(p) == \A t \in ThreadsOf(p) : pc[t] = "outside"

TypeOK ==
    /\ st    \in [Threads -> {"running", "sleeping"}]
    /\ pc    \in [Threads -> {"run", "exitspin", "counted", "handoff", "unwindwake",
                             "recheck", "park", "parked", "outside"}]
    /\ spin  \in [Threads -> BOOLEAN]
    /\ nspin \in [Pools -> 0..(N + 1)]
    /\ nsleep \in [Pools -> 0..N]
    /\ nrun  \in 0..(2 * N)
    /\ queue \in [Pools -> 0..Inject0Total]
    /\ inject \in [Pools -> 0..Inject0Total]
    /\ prewake \subseteq Threads
    /\ Cardinality(prewake) <= 1
    /\ handoffs \subseteq Pools
    /\ held  \in [Threads -> BOOLEAN]

(* The n_sleeping counter covers every sleeping worker, plus workers that   *)
(* have counted themselves but not yet published.                          *)
SleepCountOK ==
    \A p \in Pools : nsleep[p] = Cardinality({ t \in ThreadsOf(p) :
                                   st[t] = "sleeping" \/ pc[t] = "counted" })

(* The n_spinning counter always agrees with the slots actually held. *)
SpinCountOK ==
    \A p \in Pools : nspin[p] = Cardinality({ t \in ThreadsOf(p) : spin[t] })
                              + Cardinality(prewake \cap ThreadsOf(p))

Init ==
    /\ st    = [t \in Threads |-> "running"]
    /\ pc    = [t \in Threads |-> "run"]
    /\ spin  = [t \in Threads |-> FALSE]
    /\ nspin = [p \in Pools |-> 0]
    /\ nsleep = [p \in Pools |-> 0]
    /\ nrun  = N
    /\ queue = [p \in Pools |-> 0]
    /\ inject = [p \in Pools |-> Inject0[p]]
    /\ prewake = {}
    /\ handoffs = {}
    /\ held  = [t \in Threads |-> FALSE]

----------------------------------------------------------------------------
(* Wake (at most) one worker in pool `p` whose sleep_check_state is sleeping. *)

CanWakeIn(p) == \E t \in ThreadsOf(p) : st[t] = "sleeping"

(* nsleep after worker t flips from sleeping to running *)
Unsleep(t) == [nsleep EXCEPT ![Pool[t]] = @ - 1]

(* Wake one sleeping worker: flip it to running, bump the running count,     *)
(* release it if parked, and start it as a searcher. These wakes complete in  *)
(* one action; PreaccountWake below adds a competing wake whose increment,   *)
(* CAS, and rollback are separate steps, exposing temporary overcounting.    *)
WakeOne(p) ==
    \E t \in ThreadsOf(p) :
        /\ st[t] = "sleeping"
        /\ st'   = [st EXCEPT ![t] = "running"]
        /\ nrun' = nrun + 1
        /\ pc'   = [pc EXCEPT ![t] = IF pc[t] = "parked" THEN "run" ELSE @]
        /\ spin' = [spin EXCEPT ![t] = TRUE]
        /\ nspin' = [nspin EXCEPT ![p] = @ + 1]
        /\ nsleep' = Unsleep(t)

(* The gate: wake nobody while no worker is counted as sleeping, or while     *)
(* someone searches the pool; otherwise wake at most one sleeper.             *)
Gated(p) == nsleep[p] = 0 \/ nspin[p] > 0 \/ ~CanWakeIn(p)

Wakeup(p) ==
    IF Gated(p)
        THEN UNCHANGED <<st, pc, nrun, spin, nspin, nsleep>>
        ELSE WakeOne(p)

(* One additional targeted wake may be in flight, racing both the worker's  *)
(* self-flip and other wakes. Bounding this to one keeps TLC's instance small *)
(* while exposing a count that suppresses enqueues but has no worker owner.  *)
PreaccountWake ==
    /\ prewake = {}
    /\ \E t \in Threads :
        /\ st[t] = "sleeping"
        /\ prewake' = {t}
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ + 1]
    /\ UNCHANGED <<st, pc, spin, nsleep, nrun, queue, inject, handoffs, held>>

CommitWake ==
    \E t \in prewake :
        /\ st[t] = "sleeping"
        /\ st' = [st EXCEPT ![t] = "running"]
        /\ nrun' = nrun + 1
        /\ pc' = [pc EXCEPT ![t] = IF pc[t] = "parked" THEN "run" ELSE @]
        /\ spin' = [spin EXCEPT ![t] = TRUE]
        /\ prewake' = {}
        /\ nsleep' = Unsleep(t)
        /\ UNCHANGED <<nspin, queue, inject, handoffs, held>>

RollbackWake ==
    \E t \in prewake :
        /\ st[t] = "running"
        /\ prewake' = {}
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ - 1]
        /\ handoffs' = IF nspin[Pool[t]] = 1 /\ RollbackHandsOff
                           THEN handoffs \cup {Pool[t]} ELSE handoffs
        /\ UNCHANGED <<st, pc, spin, nsleep, nrun, queue, inject, held>>

RollbackHandoff ==
    \E p \in handoffs :
        /\ handoffs' = handoffs \ {p}
        /\ Wakeup(p)
        /\ UNCHANGED <<queue, inject, prewake, held>>

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
        /\ UNCHANGED held

(* A libuv callback enqueues while its worker is in the sleep transition.    *)
(* The enqueue first wakes the worker itself (wake_self), counting it as a   *)
(* searcher, then runs the gate, possibly waking another worker w.           *)
ProduceParking ==
    \E t \in Threads, p \in Pools :
        /\ pc[t] = "park"
        /\ st[t] = "sleeping"
        /\ inject[p] > 0
        /\ inject' = [inject EXCEPT ![p] = @ - 1]
        /\ queue'  = [queue EXCEPT ![p] = @ + 1]
        /\ LET ns == [nspin EXCEPT ![Pool[t]] = @ + 1]
               sl == [nsleep EXCEPT ![Pool[t]] = @ - 1]
               cand == { w \in ThreadsOf(p) : w # t /\ st[w] = "sleeping" }
           IN IF sl[p] = 0 \/ ns[p] > 0 \/ cand = {}
                 THEN /\ st'     = [st EXCEPT ![t] = "running"]
                      /\ spin'   = [spin EXCEPT ![t] = TRUE]
                      /\ nrun'   = nrun + 1
                      /\ nspin'  = ns
                      /\ nsleep' = sl
                      /\ UNCHANGED pc
                 ELSE \E w \in cand :
                      /\ st'     = [st EXCEPT ![t] = "running", ![w] = "running"]
                      /\ spin'   = [spin EXCEPT ![t] = TRUE, ![w] = TRUE]
                      /\ nrun'   = nrun + 2
                      /\ nspin'  = [ns EXCEPT ![p] = @ + 1]
                      /\ nsleep' = [sl EXCEPT ![p] = @ - 1]
                      /\ pc'     = [pc EXCEPT ![w] = IF pc[w] = "parked" THEN "run" ELSE @]
        /\ UNCHANGED held

(* Consumer fast path: a running, non-spinning worker pops a task. A worker   *)
(* that owes the exit handoff runs it.                                        *)
Consume ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ ~spin[t]
        /\ queue[Pool[t]] > 0
        /\ queue' = [queue EXCEPT ![Pool[t]] = @ - 1]
        /\ pc'    = [pc EXCEPT ![t] = IF held[t] THEN "handoff" ELSE @]
        /\ held'  = [held EXCEPT ![t] = FALSE]
        /\ UNCHANGED <<st, spin, nspin, nsleep, nrun, inject>>

(* A worker that found no work takes a searcher slot while one is free. The  *)
(* implementation admits at most half the pool unless the worker's last       *)
(* search was useful; admitting up to the whole pool covers both cases.       *)
SpinEnter ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ st[t] = "running"
        /\ ~spin[t]
        /\ nspin[Pool[t]] < Cardinality(ThreadsOf(Pool[t]))
        /\ spin'  = [spin EXCEPT ![t] = TRUE]
        /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ + 1]
        /\ held'  = [held EXCEPT ![t] = FALSE]
        /\ UNCHANGED <<st, pc, nsleep, nrun, queue, inject>>

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
        /\ UNCHANGED <<st, nsleep, nrun, inject, held>>

(* The handoff is its own step, after the release and a fence, so other       *)
(* threads may interleave before it runs. It checks the queue and wakes a     *)
(* sleeper if work remains and nobody searches.                               *)
HandoffWake ==
    \E t \in Threads :
        /\ pc[t] = "handoff"
        /\ LET p == Pool[t] IN
           IF queue[p] = 0 \/ Gated(p)
               THEN /\ pc' = [pc EXCEPT ![t] = "run"]
                    /\ UNCHANGED <<st, nrun, spin, nspin, nsleep>>
               ELSE \E w \in ThreadsOf(p) :
                       /\ st[w] = "sleeping"
                       /\ st'   = [st EXCEPT ![w] = "running"]
                       /\ nrun' = nrun + 1
                       /\ spin'  = [spin EXCEPT ![w] = TRUE]
                       /\ nspin' = [nspin EXCEPT ![p] = @ + 1]
                       /\ nsleep' = Unsleep(w)
                       /\ pc'   = [pc EXCEPT ![t] = "run",
                                              ![w] = IF pc[w] = "parked" THEN "run" ELSE pc[w]]
        /\ UNCHANGED <<queue, inject, held>>

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
        /\ held'  = [held EXCEPT ![t] = TRUE]
        /\ UNCHANGED <<st, nsleep, nrun, queue, inject>>

(* An exception unwinds a searcher out of the scheduler (trypoptask throws).  *)
(* Unlike SpinExit there is no post-publish recheck, so the pool's last       *)
(* searcher owes the same exit handoff (UnwindHandsOff = FALSE omits it).     *)
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
        /\ UNCHANGED <<st, nsleep, nrun, queue, inject, held>>

(* The unwind-path handoff (the wake in the outer JL_CATCH). It cannot check  *)
(* the queue, so it wakes a sleeper if nobody searches.                       *)
UnwindHandoff ==
    \E t \in Threads :
        /\ pc[t] = "unwindwake"
        /\ LET p == Pool[t] IN
           IF Gated(p)
               THEN /\ pc' = [pc EXCEPT ![t] = "outside"]
                    /\ UNCHANGED <<st, nrun, spin, nspin, nsleep>>
               ELSE \E w \in ThreadsOf(p) :
                       /\ st[w] = "sleeping"
                       /\ st'   = [st EXCEPT ![w] = "running"]
                       /\ nrun' = nrun + 1
                       /\ spin'  = [spin EXCEPT ![w] = TRUE]
                       /\ nspin' = [nspin EXCEPT ![p] = @ + 1]
                       /\ nsleep' = Unsleep(w)
                       /\ pc'   = [pc EXCEPT ![t] = "outside",
                                              ![w] = IF pc[w] = "parked" THEN "run" ELSE pc[w]]
        /\ UNCHANGED <<queue, inject, held>>

(* A worker owing a wake throws before its next pop (trypoptask). Its        *)
(* handler owes the same wake as an unwinding last searcher                  *)
(* (OwedUnwindHandsOff = FALSE omits it).                                    *)
ThrowOwed ==
    \E t \in Threads :
        /\ pc[t] = "run"
        /\ held[t]
        /\ pc'   = [pc EXCEPT ![t] = IF OwedUnwindHandsOff THEN "unwindwake" ELSE "outside"]
        /\ held' = [held EXCEPT ![t] = FALSE]
        /\ UNCHANGED <<st, spin, nspin, nsleep, nrun, queue, inject>>

(* Count the sleeper before PUBLISH: ex-searchers from "exitspin", and       *)
(* workers refused a slot from "run", which happens only while someone      *)
(* searches (the cap is at least one slot).                                 *)
SleepCount ==
    \E t \in Threads :
        /\ \/ pc[t] = "exitspin"
           \/ pc[t] = "run" /\ nspin[Pool[t]] > 0
        /\ st[t] = "running"
        /\ ~spin[t]
        /\ nsleep' = [nsleep EXCEPT ![Pool[t]] = @ + 1]
        /\ pc' = [pc EXCEPT ![t] = "counted"]
        /\ held' = [held EXCEPT ![t] = pc[t] = "exitspin"]
        /\ UNCHANGED <<st, spin, nspin, nrun, queue, inject>>

(* PUBLISH *)
SleepBegin ==
    \E t \in Threads :
        /\ pc[t] = "counted"
        /\ st[t] = "running"
        /\ st' = [st EXCEPT ![t] = "sleeping"]
        /\ pc' = [pc EXCEPT ![t] = "recheck"]
        /\ UNCHANGED <<spin, nspin, nsleep, nrun, queue, inject, held>>

(* RECHECK: abort the sleep if work appeared. Mirrors set_not_sleeping: a    *)
(* self-flip leaves the counter alone; a raced flip consumes the waker's      *)
(* in-flight increment and takes its slot. A self-flipping ex-searcher keeps  *)
(* `held`: enqueues may have skipped their wakes on its old slot, so it owes  *)
(* the exit handoff if its next pop finds work.                               *)
SleepRecheckAbortSelf ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ st[t] = "sleeping"
        /\ queue[Pool[t]] > 0
        /\ st' = [st EXCEPT ![t] = "running"]
        /\ pc' = [pc EXCEPT ![t] = "run"]
        /\ nsleep' = Unsleep(t)
        /\ UNCHANGED <<spin, nspin, nrun, queue, inject, held>>

SleepRecheckAbortRaced ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ st[t] = "running"          \* a waker flipped us and incremented
        /\ queue[Pool[t]] > 0
        /\ nrun' = nrun - 1           \* consume the in-flight wakeup
        /\ pc' = [pc EXCEPT ![t] = "run"]
        /\ held' = [held EXCEPT ![t] = FALSE]
        /\ UNCHANGED <<st, spin, nspin, nsleep, queue, inject>>

SleepRecheckEmpty ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ queue[Pool[t]] = 0 \/ (~held[t] /\ ~RefusedRechecks)
        /\ pc' = [pc EXCEPT ![t] = "park"]
        /\ held' = [held EXCEPT ![t] = FALSE]
        /\ UNCHANGED <<st, spin, nspin, nsleep, nrun, queue, inject>>

(* A throw during RECHECK (checkempty is a Julia callback). The handler      *)
(* settles the wake -- a self-flip, or consuming a racing waker's increment   *)
(* and releasing the slot it pre-accounted -- and then hands off through the  *)
(* gated wake ("unwindwake"): an enqueue suppressed by the slot released at   *)
(* RELEASE is observed only by the recheck, which never completed.            *)
ThrowRecheck ==
    \E t \in Threads :
        /\ pc[t] = "recheck"
        /\ IF st[t] = "sleeping"
              THEN /\ st' = [st EXCEPT ![t] = "running"]
                   /\ nsleep' = Unsleep(t)
                   /\ UNCHANGED <<nrun, spin, nspin>>
              ELSE /\ nrun' = nrun - 1
                   /\ IF spin[t]
                         THEN /\ spin'  = [spin EXCEPT ![t] = FALSE]
                              /\ nspin' = [nspin EXCEPT ![Pool[t]] = @ - 1]
                         ELSE UNCHANGED <<spin, nspin>>
                   /\ UNCHANGED <<st, nsleep>>
        /\ pc' = [pc EXCEPT ![t] = "unwindwake"]
        /\ held' = [held EXCEPT ![t] = FALSE]
        /\ UNCHANGED <<queue, inject>>

(* RETIRE + PARK: leave the running count, then commit unless a waker won    *)
(* the race (its nrun++ balances our decrement).                              *)
ParkCommit ==
    \E t \in Threads :
        /\ pc[t] = "park"
        /\ st[t] = "sleeping"
        /\ nrun' = nrun - 1
        /\ pc'   = [pc EXCEPT ![t] = "parked"]
        /\ UNCHANGED <<st, spin, nspin, nsleep, queue, inject, held>>

ParkRaced ==
    \E t \in Threads :
        /\ pc[t] = "park"
        /\ st[t] = "running"        \* a waker won the race during the window
        /\ nrun' = nrun - 1         \* consume our own pre-park decrement...
        /\ pc'   = [pc EXCEPT ![t] = "run"]
        /\ UNCHANGED <<st, spin, nspin, nsleep, queue, inject, held>>

WorkerStep ==
    \/ Produce
    \/ ProduceParking
    \/ Consume
    \/ SpinEnter
    \/ ConsumeSpinner
    \/ HandoffWake
    \/ SpinExit
    \/ ThrowSpinner
    \/ ThrowOwed
    \/ UnwindHandoff
    \/ SleepCount
    \/ SleepBegin
    \/ SleepRecheckAbortSelf
    \/ SleepRecheckAbortRaced
    \/ SleepRecheckEmpty
    \/ ThrowRecheck
    \/ ParkCommit
    \/ ParkRaced
    \* Quiescence with empty queues is legitimate (sleeping producers never
    \* inject their remaining tasks). Allowing a stutter step there makes
    \* TLC's deadlock detector fire only when work is stuck in a queue.
    \/ ((\A p \in Pools : queue[p] = 0 \/ AllOutside(p)) /\ UNCHANGED vars)

Next ==
    \/ (WorkerStep /\ UNCHANGED <<prewake, handoffs>>)
    \/ PreaccountWake
    \/ CommitWake
    \/ RollbackWake
    \/ RollbackHandoff

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

----------------------------------------------------------------------------
(* Properties. *)

(* Everything that was queued has been consumed. *)
Done == QueueEmpty /\ \A p \in Pools : inject[p] = 0

(* A non-empty queue always has an unblocked worker in its pool, except the    *)
(* AllOutside case that re-entry heals, or an outstanding wake obligation.      *)
(* Once provisional wakes and rollback handoffs finish, queued work must have *)
(* a worker able to service it.                                                *)
NoLostWakeup ==
    \A p \in Pools :
        (queue[p] > 0) =>
            \/ \E t \in ThreadsOf(p) : ~Blocked(t)
            \/ AllOutside(p)
            \/ prewake \cap ThreadsOf(p) # {}
            \/ p \in handoffs

(* Queued work never waits for busy workers while one of the pool's workers   *)
(* is parked, unless a worker of the pool will still look at the queue: a     *)
(* searcher, a worker in its sleep transition before the recheck, one that    *)
(* owes or runs a handoff, or an outstanding wake obligation.                 *)
Looking(t) ==
    \/ spin[t]
    \/ pc[t] \in {"exitspin", "counted", "recheck", "handoff", "unwindwake"}
    \/ pc[t] = "run" /\ held[t]

NoStrandedWork ==
    \A p \in Pools :
        (queue[p] > 0 /\ \E t \in ThreadsOf(p) : pc[t] = "parked") =>
            \/ \E t \in ThreadsOf(p) : Looking(t)
            \/ prewake \cap ThreadsOf(p) # {}
            \/ p \in handoffs

=============================================================================
