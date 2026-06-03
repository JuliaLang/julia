------------------------------- MODULE MCFixed -------------------------------
(* Concrete instance for TLC: same scenario as MCBuggy but with the early-out  *)
(* removed (EarlyOut = FALSE), i.e. the shipped fix that always scans each      *)
(* worker's sleep_check_state. TLC reports no deadlock and NoLostWakeup holds.  *)
(*                                                                             *)
(* A slightly larger pool (two workers in pool B, plus producers in A) gives    *)
(* the model checker room to exercise wake-one against multiple sleepers.       *)
EXTENDS Naturals, FiniteSets, TLC

Threads  == {1, 2, 3}
Pool     == (1 :> "A") @@ (2 :> "B") @@ (3 :> "B")
Inject0  == ("A" :> 0) @@ ("B" :> 2)
EarlyOut == FALSE

VARIABLES st, pc, nrun, queue, inject

INSTANCE SchedulerWake
=============================================================================
