----------------------- MODULE MCBuggyEarlyValidation -----------------------
(* NEGATIVE test: the bare re-declaration validates the retained slot value  *)
(* BEFORE activating the re-type guards and draining the in-flight commit    *)
(* windows (EarlyBareValidation = TRUE) -- the ordering bug found while       *)
(* reviewing the PR. A storer whose value was legally validated against the  *)
(* still-published weaker restriction (Any) can commit between the           *)
(* validation and the drain; the re-declaration then publishes the new       *)
(* restriction (Int) on the strength of its stale verdict, leaving a         *)
(* non-conforming retained value published.                                  *)
(*                                                                           *)
(* TLC must FIND a violation of TypeSlotOK (equivalently BareRetypeSound's   *)
(* success conjunct): slot = "f" while the published restriction is Int.     *)
(* Note StoreValidation still HOLDS here -- every individual store was legal  *)
(* when it committed; it is the publication that is broken.                  *)
EXTENDS Naturals, Sequences, FiniteSets, TLC

Script == << [ty |-> "Int",   val |-> "none"],
             [ty |-> "Float", val |-> "f"],
             [ty |-> "Int",   val |-> "none"] >>
StoreAttempts == 2
ReadAttempts == 2
StorerWorldCheck == TRUE
EarlyBareValidation == TRUE

VARIABLES world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld, window,
          badStore, rs, cc, rd, rt

INSTANCE BindingRetype
=============================================================================
