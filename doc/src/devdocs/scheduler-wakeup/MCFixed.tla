------------------------------- MODULE MCFixed -------------------------------
(* Concrete instance for TLC: the shipped wake-one algorithm that always scans  *)
(* each worker's sleep_check_state. TLC reports no deadlock and NoLostWakeup     *)
(* holds across the full state space.                                           *)
(*                                                                             *)
(* Two workers share pool B (plus a producer in A) so the model checker can     *)
(* exercise wake-one against multiple sleepers, including cross-pool inserts.    *)
EXTENDS Naturals, FiniteSets, TLC

Threads  == {1, 2, 3}
Pool     == (1 :> "A") @@ (2 :> "B") @@ (3 :> "B")
Inject0  == ("A" :> 0) @@ ("B" :> 2)

VARIABLES st, pc, nrun, queue, inject

INSTANCE SchedulerWake
=============================================================================
