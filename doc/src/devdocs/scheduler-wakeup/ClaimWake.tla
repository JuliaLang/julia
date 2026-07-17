---------------------------- MODULE ClaimWake ----------------------------
(***************************************************************************)
(* The claim protocol for tid-pinned tasks in the workstealing scheduler    *)
(* (base/scheduler/workstealing.jl `tryclaim`), and its interaction with    *)
(* the spinner-gated wakeup policy of JuliaLang/julia#62284.                 *)
(*                                                                           *)
(* A task blocked in wait()/take! *hosts* its thread's sleep logic: it       *)
(* remains that thread's current task and its tid stays pinned while the     *)
(* thread parks, so only that thread may run it when it is rescheduled.      *)
(* Any other dequeuer's claim fails. The question this module checks is      *)
(* what the failing dequeuer must do with the task:                          *)
(*                                                                           *)
(*   ClaimPolicy = "reinject": re-park it in the shared injection queue and  *)
(*     run the pool wakeup policy. The pool wake is gated on the pool's      *)
(*     spinner count — and the failing dequeuer is typically itself the      *)
(*     spinner holding that slot. The pinned thread, once parked, is never   *)
(*     woken again, and the task cycles between the dequeuer and the queue   *)
(*     forever: a livelock.                                                   *)
(*                                                                           *)
(*   ClaimPolicy = "mailbox": deliver the task to the pinned thread's own    *)
(*     sticky workqueue and wake that thread *directly* (targeted wakes      *)
(*     are not spinner-gated). The task leaves the shared queues, so no      *)
(*     dequeuer can bounce it again.                                          *)
(*                                                                           *)
(* Because the failure is a livelock, not a stuck state, the property is a   *)
(* liveness one — the pinned task eventually runs — under weak fairness of   *)
(* each action (no thread is suspended forever by the scheduler while it     *)
(* has an enabled step). The reinject policy fails it with a cycle: the      *)
(* owner parks, all of its actions become disabled (fairness is vacuous for  *)
(* a parked thread), and the thief loops pop -> fail forever.                *)
(*                                                                           *)
(* Model shape: thread 1 ("owner") hosts the pinned task W and runs the      *)
(* sleep protocol of SchedulerWake.tla in reduced form (its post-publish     *)
(* re-check is the post-fence get_next_task retry in jl_task_get_next).      *)
(* Thread 2 ("thief") holds the pool's only spinner slot throughout —        *)
(* pool of two, cap of one — which is both realistic (persistent idle        *)
(* spinner) and the configuration that gates the reinject wake. The          *)
(* schedule-side targeted wake of cd463b86b9 (#62371) is included: the      *)
(* livelock needs the owner to *miss* that one wake (W in the thief's        *)
(* hands during the owner's re-check), which TLC finds.                      *)
(***************************************************************************)
EXTENDS Naturals

CONSTANT ClaimPolicy    \* "mailbox" (the implementation) or "reinject"
                        \* (omits the direct wake; violates WEventuallyDone)

VARIABLES
    ost,    \* owner sleep_check_state: "running" | "sleeping"
    opc,    \* owner pc: "run" | "recheck" | "parked"
    wloc    \* W: "unscheduled" | "queued" | "held" | "mailbox" | "done"

vars == <<ost, opc, wloc>>

TypeOK ==
    /\ ost \in {"running", "sleeping"}
    /\ opc \in {"run", "recheck", "parked"}
    /\ wloc \in {"unscheduled", "queued", "held", "mailbox", "done"}

Init == ost = "running" /\ opc = "run" /\ wloc = "unscheduled"

(* A targeted wake of the owner (jl_wakeup_thread): not spinner-gated. *)
OwnerWoken(ost2, opc2) ==
    IF ost2 = "sleeping" THEN <<"running", IF opc2 = "parked" THEN "run" ELSE opc2>>
                         ELSE <<ost2, opc2>>

----------------------------------------------------------------------------
(* The producer (running elsewhere) schedules W into the shared queue and    *)
(* direct-wakes the pinned owner (#62371's enq_work behavior).               *)
Produce ==
    /\ wloc = "unscheduled"
    /\ wloc' = "queued"
    /\ LET w == OwnerWoken(ost, opc) IN ost' = w[1] /\ opc' = w[2]

(* The thief (the pool's spinner) pops W from the shared queue; its claim    *)
(* will fail, but while it holds W the task is invisible to the owner.       *)
ThiefPop ==
    /\ wloc = "queued"
    /\ wloc' = "held"
    /\ UNCHANGED <<ost, opc>>

(* The claim fails (W is pinned to the owner). What happens next is the      *)
(* policy under test.                                                        *)
ThiefFail ==
    /\ wloc = "held"
    /\ IF ClaimPolicy = "mailbox"
        THEN /\ wloc' = "mailbox"
             /\ LET w == OwnerWoken(ost, opc) IN ost' = w[1] /\ opc' = w[2]
        ELSE \* reinject + pool wake, gated by the thief's own spinner slot:
             \* wakes nobody
             /\ wloc' = "queued"
             /\ UNCHANGED <<ost, opc>>

----------------------------------------------------------------------------
(* Owner: claim W. From the shared queue this races the thief; the mailbox   *)
(* is the owner's own workqueue, which no one else pops.                      *)
OwnerClaim ==
    /\ opc \in {"run", "recheck"}
    /\ wloc \in {"queued", "mailbox"}
    /\ wloc' = "done"
    /\ ost' = "running" /\ opc' = "run"

(* Owner heads to sleep: publish sleep_check_state, then the post-fence      *)
(* re-check (the get_next_task retry inside the sleep block).                *)
OwnerSleepBegin ==
    /\ opc = "run" /\ ost = "running"
    /\ wloc /= "done"
    /\ ost' = "sleeping" /\ opc' = "recheck"
    /\ UNCHANGED wloc

(* Re-check found nothing visible (W unscheduled, held by the thief, or      *)
(* already in the mailbox *after* a wake we will still observe): park unless *)
(* a waker raced us back to running.                                          *)
OwnerParkCommit ==
    /\ opc = "recheck" /\ ost = "sleeping"
    /\ wloc \notin {"queued", "mailbox"}   \* visible work aborts the sleep via OwnerClaim
    /\ opc' = "parked"
    /\ UNCHANGED <<ost, wloc>>

OwnerParkRaced ==
    /\ opc = "recheck" /\ ost = "running"  \* a targeted wake raced the park
    /\ opc' = "run"
    /\ UNCHANGED <<ost, wloc>>

Next ==
    \/ Produce
    \/ ThiefPop
    \/ ThiefFail
    \/ OwnerClaim
    \/ OwnerSleepBegin
    \/ OwnerParkCommit
    \/ OwnerParkRaced
    \/ (wloc = "done" /\ UNCHANGED vars)   \* quiescence

(* Weak fairness per action: any continuously enabled step eventually runs.  *)
(* A parked owner has no enabled steps, so fairness cannot save it — only a  *)
(* wake can, which is the point.                                              *)
Spec == Init /\ [][Next]_vars
             /\ WF_vars(Produce) /\ WF_vars(ThiefPop) /\ WF_vars(ThiefFail)
             /\ WF_vars(OwnerClaim) /\ WF_vars(OwnerSleepBegin)
             /\ WF_vars(OwnerParkCommit) /\ WF_vars(OwnerParkRaced)

(* The pinned task is eventually run by its owner. Fails for "reinject"      *)
(* with the cycle: owner parked, thief looping held <-> queued.              *)
WEventuallyDone == <>(wloc = "done")

=============================================================================
