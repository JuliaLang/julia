------------------------ MODULE MCBuggyEarlyValidation ------------------------
(* Negative model: the bare re-declaration's retained-value validation is    *)
(* performed before the guard flagging and drain (the ordering bug found in  *)
(* review of the PR), so a storer that validated against the still-published *)
(* weak epoch can commit an "f" between the frozen verdict and the           *)
(* publication of Int. TLC must find a TypeSlotOK violation.                 *)
EXTENDS Naturals, Sequences, FiniteSets, TLC

Script == << [ty |-> "Int",   val |-> "none"],
             [ty |-> "Any",   val |-> "none"],
             [ty |-> "Float", val |-> "f"] >>
StoreAttempts == 2
ReadAttempts == 2
SwappedFlagConditions == FALSE
EarlyBareValidation == TRUE

VARIABLES eps, pub, slot, pendR, pendW, lockHeld, window, badStore, rs, cc, rd, rt

INSTANCE BindingRetype
=============================================================================
