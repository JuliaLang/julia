------------------------ MODULE MCBuggyUnflushedWindow ------------------------
(* Negative model: the heavy fence does NOT publish buffered window opens     *)
(* (FenceFlushesWindows = FALSE) -- i.e. the "commit bit" is assumed visible  *)
(* to the drain without the barrier that actually makes it so. A storer that  *)
(* opened its window and read its write flag stale-clear then has that open   *)
(* invisible to the drain, which proceeds and publishes the narrowing while   *)
(* the commit is still in flight; the stale store then lands under the new    *)
(* (Float) restriction. TLC must find a TypeSlotOK / ClearWriteFlagsSound /   *)
(* StoreValidation violation, showing the fence's window flush is required.   *)
EXTENDS Naturals, Sequences, FiniteSets, TLC

Script == << [ty |-> "Int",   val |-> "none"],
             [ty |-> "Any",   val |-> "none"],
             [ty |-> "Float", val |-> "f"] >>
StoreAttempts == 2
ReadAttempts == 2
SwappedFlagConditions == FALSE
EarlyBareValidation == FALSE
FenceFlushesWindows == FALSE

VARIABLES eps, pub, slot, pendR, pendW, lockHeld, window, pendWin, badStore, rs, cc, rd, rt

INSTANCE BindingRetype
=============================================================================
