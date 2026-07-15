------------------------- MODULE MCBuggySwappedFlags -------------------------
(* Negative model: the per-partition flag calculus with the two subtype      *)
(* tests swapped (reads flagged when the old restriction is not a subtype of *)
(* the new one, writes vice versa) -- a plausible-looking transposition of   *)
(* the correct conditions. TLC must find violations: a widening then leaves  *)
(* stale readers trusting a slot that may hold wider values                  *)
(* (ReaderSoundness / ClearReadFlagsSound), and a narrowing leaves stale     *)
(* writers committing values the new restriction rejects (StoreValidation).  *)
EXTENDS Naturals, Sequences, FiniteSets, TLC

Script == << [ty |-> "Int",   val |-> "none"],
             [ty |-> "Any",   val |-> "none"],
             [ty |-> "Float", val |-> "f"] >>
StoreAttempts == 2
ReadAttempts == 2
SwappedFlagConditions == TRUE
EarlyBareValidation == FALSE
FenceFlushesWindows == TRUE

VARIABLES eps, pub, slot, pendR, pendW, lockHeld, window, pendWin, badStore, rs, cc, rd, rt

INSTANCE BindingRetype
=============================================================================
