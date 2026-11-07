---------------------------- MODULE IoLoopWake ----------------------------
(***************************************************************************)
(* A TLA+ model of how `jl_task_get_next` hands libuv events to the event  *)
(* loop thread (io_loop_tid, "T0" here) outside a threaded region, and of  *)
(* finalizers that take the iolock (`uvfinalize`) while doing so.          *)
(*                                                                         *)
(* Only T0 runs `uv_run`. A worker that armed a libuv handle and then goes *)
(* to sleep falls back to waking T0 if the iolock is free; if the iolock   *)
(* is held, it relies on the holder to get the loop serviced:              *)
(*                                                                         *)
(*   w1  publish sleep_check_state = sleeping, re-check the queue          *)
(*   w2  (pending finalizers may run here, from `get_next_task`)           *)
(*   w3  if jl_uv_mutex.owner == NULL: jl_wakeup_thread(io_loop_tid)       *)
(*   w4  park until woken                                                  *)
(*                                                                         *)
(* T0's sleep transition:                                                  *)
(*                                                                         *)
(*   t1  publish sleep_check_state = sleeping                              *)
(*   t2  (pending finalizers may run here, from `get_next_task`)           *)
(*   t3  JL_UV_LOCK; enter the loop only if still sleeping and no other    *)
(*       thread waits for the iolock (jl_uv_n_waiters == 0)                *)
(*   t4  uv_run(UV_RUN_ONCE), which returns whether the loop is still      *)
(*       alive (`active`)                                                  *)
(*   t5  JL_UV_UNLOCK, which runs pending finalizers                       *)
(*   t5' with Recheck, if the loop was found idle: check again under a     *)
(*       trylock, released without running finalizers; if busy, start over *)
(*   t6  if active or woken: start over; if the loop was skipped: start    *)
(*       over (T0 must stay up to run it); else park until woken           *)
(*                                                                         *)
(* A finalizer run at t5 can reacquire the iolock after `uv_run` found the *)
(* loop idle. A worker that armed a handle after the unlock and reaches w3 *)
(* while the finalizer holds the iolock skips the wake, and T0 parks at    *)
(* t6: nobody runs the loop again. With Recheck, T0 checks again that the  *)
(* loop is idle after the unlock, under a trylock that it releases without *)
(* running finalizers, as `jl_task_get_next` does.                         *)
(*                                                                         *)
(* Actions are atomic; `uv_run` delivers the events of any subset of the   *)
(* armed handles. Blocking in `uv_run` (and `jl_wake_libuv` interrupting   *)
(* it), weak memory, n_threads_running, `wait_empty` and threaded regions  *)
(* are abstracted away, and finalizers take the iolock without contending  *)
(* for it. This is a safety model: it checks that armed handles are never  *)
(* stranded with every thread blocked, not that the loop gets serviced     *)
(* while another thread keeps running.                                     *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
    Workers,            \* set of worker thread ids
    Cycles,             \* how many times each worker arms a handle
    GCs,                \* how many times a GC leaves finalizers pending
    Recheck             \* the fix: revalidate an idle loop after the unlock

T0 == "T0"
NoOwner == "none"
Threads == Workers \cup {T0}

ASSUME /\ IsFiniteSet(Workers)
       /\ T0 \notin Workers /\ NoOwner \notin Workers
       /\ Cycles \in Nat /\ GCs \in Nat
       /\ Recheck \in BOOLEAN

VARIABLES
    st,         \* st[t] in {"running", "sleeping"} -- the sleep_check_state
    pc,         \* pc[t]: the step thread t is about to take
    owner,      \* holder of jl_uv_mutex (the iolock), or NoOwner
    armed,      \* armed[w]: w's handle waits for uv_run to deliver its event
    ready,      \* ready[w]: w's task is runnable (its event was delivered)
    enter,      \* T0 decided to enter the loop at t3
    active,     \* T0's last `uv_run` result
    pending,    \* finalizers are pending (jl_gc_have_pending_finalizers)
    gcs,        \* GCs left
    cycles      \* cycles[w]: handles w has yet to arm

vars == <<st, pc, owner, armed, ready, enter, active, pending, gcs, cycles>>

T0Steps == {"t1", "t1task", "t2", "t2fin", "t3", "t4", "t5", "t5fin", "t5end",
            "t5check", "t5relock", "t6", "parked"}
WorkerSteps == {"arm", "armwait", "armed", "w1", "w1check", "w2", "w2fin", "w3",
                "parked"}

TypeOK ==
    /\ st      \in [Threads -> {"running", "sleeping"}]
    /\ pc      \in [Threads -> T0Steps \cup WorkerSteps]
    /\ pc[T0]  \in T0Steps
    /\ \A w \in Workers : pc[w] \in WorkerSteps
    /\ owner   \in Threads \cup {NoOwner}
    /\ armed   \in [Workers -> BOOLEAN]
    /\ ready   \in [Workers -> BOOLEAN]
    /\ enter   \in BOOLEAN
    /\ active  \in BOOLEAN
    /\ pending \in BOOLEAN
    /\ gcs     \in 0..GCs
    /\ cycles  \in [Workers -> 0..Cycles]

(* The iolock is held exactly by threads in a critical section. *)
LockOK ==
    /\ owner = T0 <=> pc[T0] \in {"t1task", "t2fin", "t4", "t5", "t5end", "t5relock"}
    /\ \A w \in Workers : owner = w <=> pc[w] \in {"armed", "w2fin"}

Init ==
    /\ st      = [t \in Threads |-> "running"]
    /\ pc      = [t \in Threads |-> IF t = T0 THEN "t1" ELSE "arm"]
    /\ owner   = NoOwner
    /\ armed   = [w \in Workers |-> FALSE]
    /\ ready   = [w \in Workers |-> TRUE]
    /\ enter   = FALSE
    /\ active  = FALSE
    /\ pending = FALSE
    /\ gcs     = GCs
    /\ cycles  = [w \in Workers |-> Cycles]

----------------------------------------------------------------------------
(* Helpers. *)

Goto(t, l) == pc' = [pc EXCEPT ![t] = l]

(* jl_wakeup_thread(t): flip a sleeping thread back to running. *)
Wake(t) == st' = [st EXCEPT ![t] = IF @ = "sleeping" THEN "running" ELSE @]

(* Threads waiting in JL_UV_LOCK (jl_uv_n_waiters > 0). *)
Waiters == {w \in Workers : pc[w] = "armwait"}

(* Thread t at step `from` runs the pending finalizers, which take the     *)
(* iolock (`uvfinalize`), and continues at step `mid` while holding it.    *)
FinBegin(t, from, mid) ==
    /\ pc[t] = from
    /\ pending
    /\ owner = NoOwner
    /\ owner' = t
    /\ pending' = FALSE
    /\ Goto(t, mid)
    /\ UNCHANGED <<st, armed, ready, enter, active, gcs, cycles>>

(* Thread t at step `mid` releases the iolock and continues at step `to`. *)
FinEnd(t, mid, to) ==
    /\ pc[t] = mid
    /\ owner' = NoOwner
    /\ Goto(t, to)
    /\ UNCHANGED <<st, armed, ready, enter, active, pending, gcs, cycles>>

(* Thread t at step `from` doesn't run finalizers. *)
NoFin(t, from, to) ==
    /\ pc[t] = from
    /\ Goto(t, to)
    /\ UNCHANGED <<st, owner, armed, ready, enter, active, pending, gcs, cycles>>

----------------------------------------------------------------------------
(* Some thread allocates and collects; the finalizers of the garbage cannot *)
(* run right away (the collector holds a lock, or has them disabled).       *)
GC ==
    /\ gcs > 0
    /\ gcs' = gcs - 1
    /\ pending' = TRUE
    /\ UNCHANGED <<st, pc, owner, armed, ready, enter, active, cycles>>

----------------------------------------------------------------------------
(* The event loop thread T0. *)

(* While running a task, T0 may take the iolock itself (e.g. for I/O). *)
T0TaskBegin ==
    /\ pc[T0] = "t1"
    /\ owner = NoOwner
    /\ owner' = T0
    /\ Goto(T0, "t1task")
    /\ UNCHANGED <<st, armed, ready, enter, active, pending, gcs, cycles>>

T0TaskEnd == FinEnd(T0, "t1task", "t1")

T0Sleep ==
    /\ pc[T0] = "t1"
    /\ st' = [st EXCEPT ![T0] = "sleeping"]
    /\ Goto(T0, "t2")
    /\ UNCHANGED <<owner, armed, ready, enter, active, pending, gcs, cycles>>

T0Lock ==
    /\ pc[T0] = "t3"
    /\ owner = NoOwner
    /\ owner' = T0
    /\ enter' = (st[T0] = "sleeping" /\ Waiters = {})
    /\ Goto(T0, "t4")
    /\ UNCHANGED <<st, armed, ready, active, pending, gcs, cycles>>

(* Run the loop once: deliver the events of some of the armed handles,      *)
(* scheduling and waking the tasks that wait for them. The loop stays alive *)
(* while handles remain armed, and may because of other handles.            *)
T0RunLoop ==
    /\ pc[T0] = "t4"
    /\ IF enter
          THEN \E S \in SUBSET {w \in Workers : armed[w]} :
                  /\ armed' = [w \in Workers |-> armed[w] /\ w \notin S]
                  /\ ready' = [w \in Workers |-> ready[w] \/ w \in S]
                  /\ st' = [t \in Threads |-> IF t \in S THEN "running" ELSE st[t]]
                  /\ IF \E w \in Workers : armed'[w]
                        THEN active' = TRUE
                        ELSE active' \in BOOLEAN
          ELSE /\ active' = FALSE
               /\ UNCHANGED <<st, armed, ready>>
    /\ Goto(T0, "t5")
    /\ UNCHANGED <<owner, enter, pending, gcs, cycles>>

T0Unlock ==
    /\ pc[T0] = "t5"
    /\ owner' = NoOwner
    /\ Goto(T0, "t5fin")
    /\ UNCHANGED <<st, armed, ready, enter, active, pending, gcs, cycles>>

(* With Recheck: if the loop was found idle, check again under the iolock  *)
(* (trylock), and release it without running finalizers. If the iolock is  *)
(* busy, retry.                                                            *)
T0Revalidate ==
    /\ pc[T0] = "t5check"
    /\ IF Recheck /\ enter /\ ~active
          THEN IF owner = NoOwner
                  THEN /\ owner' = T0
                       /\ active' = \E w \in Workers : armed[w]
                       /\ Goto(T0, "t5relock")
                  ELSE /\ active' = TRUE
                       /\ Goto(T0, "t6")
                       /\ UNCHANGED owner
          ELSE /\ Goto(T0, "t6")
               /\ UNCHANGED <<owner, active>>
    /\ UNCHANGED <<st, armed, ready, enter, pending, gcs, cycles>>

T0Recheck ==
    /\ pc[T0] = "t6"
    /\ IF active \/ st[T0] = "running" \/ ~enter
          THEN /\ st' = [st EXCEPT ![T0] = "running"]
               /\ Goto(T0, "t1")
          ELSE /\ Goto(T0, "parked")
               /\ UNCHANGED st
    /\ UNCHANGED <<owner, armed, ready, enter, active, pending, gcs, cycles>>

T0Resume ==
    /\ pc[T0] = "parked"
    /\ st[T0] = "running"
    /\ Goto(T0, "t1")
    /\ UNCHANGED <<st, owner, armed, ready, enter, active, pending, gcs, cycles>>

T0Next ==
    \/ T0TaskBegin \/ T0TaskEnd
    \/ T0Sleep
    \/ FinBegin(T0, "t2", "t2fin") \/ FinEnd(T0, "t2fin", "t3") \/ NoFin(T0, "t2", "t3")
    \/ T0Lock
    \/ T0RunLoop
    \/ T0Unlock
    \* JL_UV_UNLOCK runs the pending finalizers, unless there are none or they
    \* can't run on this thread
    \/ FinBegin(T0, "t5fin", "t5end") \/ FinEnd(T0, "t5end", "t5check")
    \/ NoFin(T0, "t5fin", "t5check")
    \/ T0Revalidate
    \/ FinEnd(T0, "t5relock", "t6")
    \/ T0Recheck
    \/ T0Resume

----------------------------------------------------------------------------
(* Workers. *)

(* Under the iolock, arm the handle (e.g. `uv_poll_start`, `uv_timer_start`). *)
Arm(w) ==
    /\ owner = NoOwner
    /\ owner' = w
    /\ armed' = [armed EXCEPT ![w] = TRUE]
    /\ ready' = [ready EXCEPT ![w] = FALSE]
    /\ cycles' = [cycles EXCEPT ![w] = @ - 1]
    /\ Goto(w, "armed")
    /\ UNCHANGED <<st, enter, active, pending, gcs>>

(* The worker's task runs and calls JL_UV_LOCK to arm a handle: take the     *)
(* iolock if it is free, or else wait for it (counted in jl_uv_n_waiters).    *)
WorkerLock(w) ==
    /\ pc[w] = "arm"
    /\ ready[w]
    /\ cycles[w] > 0
    /\ IF owner = NoOwner
          THEN Arm(w)
          ELSE /\ Goto(w, "armwait")
               /\ UNCHANGED <<st, owner, armed, ready, enter, active, pending, gcs, cycles>>

WorkerArm(w) == pc[w] = "armwait" /\ Arm(w)

(* JL_UV_UNLOCK (finalizers are left to w2), then the task blocks until its *)
(* event arrives.                                                           *)
WorkerUnlock(w) == FinEnd(w, "armed", "w1")

WorkerSleep(w) ==
    /\ pc[w] = "w1"
    /\ st' = [st EXCEPT ![w] = "sleeping"]
    /\ Goto(w, "w1check")
    /\ UNCHANGED <<owner, armed, ready, enter, active, pending, gcs, cycles>>

(* Re-check the queue after publishing `sleeping`; abort the sleep if the *)
(* task became runnable.                                                  *)
WorkerRecheck(w) ==
    /\ pc[w] = "w1check"
    /\ IF ready[w]
          THEN /\ st' = [st EXCEPT ![w] = "running"]
               /\ Goto(w, "arm")
          ELSE /\ Goto(w, "w2")
               /\ UNCHANGED st
    /\ UNCHANGED <<owner, armed, ready, enter, active, pending, gcs, cycles>>

(* The fallback: wake T0 unless somebody holds the iolock. *)
WorkerFallback(w) ==
    /\ pc[w] = "w3"
    /\ IF owner = NoOwner THEN Wake(T0) ELSE UNCHANGED st
    /\ Goto(w, "parked")
    /\ UNCHANGED <<owner, armed, ready, enter, active, pending, gcs, cycles>>

WorkerResume(w) ==
    /\ pc[w] = "parked"
    /\ st[w] = "running"
    /\ Goto(w, "w1")
    /\ UNCHANGED <<st, owner, armed, ready, enter, active, pending, gcs, cycles>>

WorkerNext(w) ==
    \/ WorkerLock(w)
    \/ WorkerArm(w)
    \/ WorkerUnlock(w)
    \/ WorkerSleep(w)
    \/ WorkerRecheck(w)
    \/ FinBegin(w, "w2", "w2fin") \/ FinEnd(w, "w2fin", "w3") \/ NoFin(w, "w2", "w3")
    \/ WorkerFallback(w)
    \/ WorkerResume(w)

----------------------------------------------------------------------------
(* Nothing armed and every worker is out of work: legitimately quiescent. *)
Quiescent == \A w \in Workers : ~armed[w] /\ cycles[w] = 0

Next ==
    \/ GC
    \/ T0Next
    \/ \E w \in Workers : WorkerNext(w)
    \* Once quiescent, threads may park for good; allow a stutter step there so
    \* TLC's deadlock check (which complements NoLostWakeup) only fires on
    \* states that are stuck with a handle armed.
    \/ (Quiescent /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars

----------------------------------------------------------------------------
(* Properties. *)

(* A thread is blocked once it parked and nobody woke it, or ran out of work. *)
Blocked(t) ==
    \/ pc[t] = "parked" /\ st[t] = "sleeping"
    \/ t \in Workers /\ pc[t] = "arm" /\ cycles[t] = 0

(* An armed handle cannot coexist with every thread blocked: the loop can  *)
(* only be run again if some thread is still going to wake T0 or run it.   *)
NoLostWakeup ==
    (\E w \in Workers : armed[w]) => \E t \in Threads : ~Blocked(t)

=============================================================================
