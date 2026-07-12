------------------------- MODULE MCBuggyNoWorldCheck -------------------------
(* NEGATIVE test: the runtime storer's commit window re-checks the FLAG      *)
(* ONLY (StorerWorldCheck = FALSE), as if jl_binding_begin_commit dropped     *)
(* its `jl_world_counter != validated_world` comparison. This is exactly     *)
(* what makes the *transient* guard deactivation (jl_declare_global's        *)
(* `transient_guards` path) unsound: a runtime store that validated its      *)
(* value against the weaker superseded restriction (Any) and stalled before  *)
(* its window no longer observes the flag after the re-declaration cleared   *)
(* it, and lands its stale value after the new restriction was published.    *)
(*                                                                           *)
(* TLC must FIND a violation of TypeSlotOK (and StoreValidation): slot = "f" *)
(* while the published restriction is Int.                                   *)
EXTENDS Naturals, Sequences, FiniteSets, TLC

Script == << [ty |-> "Int",   val |-> "none"],
             [ty |-> "Float", val |-> "f"],
             [ty |-> "Int",   val |-> "none"] >>
StoreAttempts == 2
ReadAttempts == 2
StorerWorldCheck == FALSE
EarlyBareValidation == FALSE

VARIABLES world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld, window,
          badStore, rs, cc, rd, rt

INSTANCE BindingRetype
=============================================================================
