-------------------------- MODULE MCBindingRetype --------------------------
(* Concrete instance for TLC: the shipped per-partition protocol. The script *)
(* exercises the full flag matrix over the Int/Float/Any lattice:            *)
(*   1. bare `global x::Int` over the weak (Any) epoch -- flags the weak     *)
(*      epoch's write guard only (Int <: Any: reads stay trusted); may fail  *)
(*      validation if a storer put an "f" in the slot first;                 *)
(*   2. bare `global x::Any` -- a pure widening: flags the Int epoch's read  *)
(*      guard only (its writes stay direct: an Int conforms to Any), so no   *)
(*      drain runs;                                                          *)
(*   3. `global x::Float = f` -- value-carrying narrowing: flags write        *)
(*      guards on the Any epochs and both guards on the Int epoch.           *)
(* All invariants hold and every fair behavior terminates.                   *)
EXTENDS Naturals, Sequences, FiniteSets, TLC

Script == << [ty |-> "Int",   val |-> "none"],
             [ty |-> "Any",   val |-> "none"],
             [ty |-> "Float", val |-> "f"] >>
StoreAttempts == 2
ReadAttempts == 2
SwappedFlagConditions == FALSE
EarlyBareValidation == FALSE

VARIABLES eps, pub, slot, pendR, pendW, lockHeld, window, badStore, rs, cc, rd, rt

INSTANCE BindingRetype
=============================================================================
