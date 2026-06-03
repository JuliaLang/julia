------------------------------- MODULE MCBuggy -------------------------------
(* Concrete instance for TLC: two pools, one worker each, a single task        *)
(* injected cross-pool, with the buggy n_threads_running early-out enabled.    *)
(* TLC finds a deadlock / NoLostWakeup violation: the cross-pool insert reads   *)
(* "all threads running" (the target worker is mid-sleep-transition and still   *)
(* counted) and skips the wake, stranding the task.                            *)
EXTENDS Naturals, FiniteSets, TLC

Threads  == {1, 2}
Pool     == (1 :> "A") @@ (2 :> "B")
Inject0  == ("A" :> 0) @@ ("B" :> 1)
EarlyOut == TRUE

VARIABLES st, pc, nrun, queue, inject

INSTANCE SchedulerWake
=============================================================================
