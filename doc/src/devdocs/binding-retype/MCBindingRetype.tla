-------------------------- MODULE MCBindingRetype --------------------------
(* Concrete instance for TLC: the shipped protocol (world check in the       *)
(* runtime storer's commit window, retained-value validation after the       *)
(* drain). All invariants hold and every fair behavior terminates.           *)
(*                                                                           *)
(* The re-typer's script covers the three interesting declaration shapes:    *)
(*   1. bare `global x::Int` over the weak (Any) binding -- a `transition`    *)
(*      that activates the guards, may fail validation (a storer can put an   *)
(*      "f" in the slot first), and transiently deactivates on success;       *)
(*   2. `global x::Float = f` -- value-carrying; diverged (sticky flag) when  *)
(*      declaration 1 published Int, itself a transient transition otherwise; *)
(*   3. bare `global x::Int` again -- re-entry with guards already active     *)
(*      (or a fresh diverged activation), whose validation failure must roll  *)
(*      back without publishing and without clearing a sticky flag.           *)
EXTENDS Naturals, Sequences, FiniteSets, TLC

Script == << [ty |-> "Int",   val |-> "none"],
             [ty |-> "Float", val |-> "f"],
             [ty |-> "Int",   val |-> "none"] >>
StoreAttempts == 2
ReadAttempts == 2
StorerWorldCheck == TRUE
EarlyBareValidation == FALSE

VARIABLES world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld, window,
          badStore, rs, cc, rd, rt

INSTANCE BindingRetype
=============================================================================
