--------------------------- MODULE BindingRetype ---------------------------
(***************************************************************************)
(* A TLA+ model of Julia's global-binding re-typing protocol                *)
(* (issue #62154, PR #62335): the race between stores to a global           *)
(* binding's single value slot, compiled fast-path reads of that slot, and  *)
(* a re-declaration (`global x::T [= v]`) that replaces the binding's       *)
(* declared type while such accesses are in flight.                         *)
(*                                                                          *)
(* The code being modeled:                                                  *)
(*   - src/julia_internal.h: jl_binding_begin_commit / jl_binding_end_commit *)
(*   - src/module.c: jl_activate_retype_guards, jl_deactivate_retype_guards, *)
(*     jl_checked_assignment (and the other jl_checked_* storers)            *)
(*   - src/toplevel.c: jl_declare_global (divergence scan, `transition`,     *)
(*     guard activation + drain, retained-value validation, value swap,      *)
(*     world publication, transient deactivation)                            *)
(*   - src/cgutils.cpp: emit_retype_recheck, emit_binding_commit_begin/end;  *)
(*     src/codegen.cpp: emit_retype_guard, emit_globalref                    *)
(*                                                                          *)
(* Shared state per binding: a value `slot` (each value has a runtime       *)
(* type), the BINDING_FLAG_RETYPED flag, and a *published* declared type    *)
(* ("restriction") governed by the monotonically increasing world counter.  *)
(* world_counter_lock serializes re-declarations and the "locked" store     *)
(* path. The invariant maintained: the slot's value always conforms to the  *)
(* currently published restriction (TypeSlotOK below).                      *)
(*                                                                          *)
(* The threads:                                                             *)
(*                                                                          *)
(* Storer (runtime path, jl_checked_assignment):                            *)
(*   s1  vw := world; validate v against the restriction published at vw    *)
(*       (error if non-conforming). Modeled as one atomic step: in the C     *)
(*       code the world capture and the validation's own world load are      *)
(*       distinct, but a store only commits if world still equals vw at s4,  *)
(*       so any interleaving in which the world moved between the two loads  *)
(*       diverts to the locked path anyway; collapsing them loses no          *)
(*       distinguishable committed behavior. The thread may stall arbitrarily *)
(*       long between s1 and s3 (TLC interleaving covers this).              *)
(*   s3  open the commit window: ptls->bnd_commit_window := 1                *)
(*   s4  read flag and world: if flag set OR world # vw, close the window    *)
(*       and take the LOCKED path (re-validate against the *latest*          *)
(*       restriction under world_counter_lock, then store)                   *)
(*   s5  commit: slot := v          (while the window is still open)         *)
(*   s6  close the window (release; pairs with the drain's acquire)          *)
(*                                                                          *)
(* Compiled fast-path storer: same shape but s4 checks the flag ONLY (no    *)
(* world check), and its validation is against the declared type Tc its     *)
(* code was compiled for. Compiled fast paths exist only for epochs whose   *)
(* partition kind is a typed GLOBAL (PARTITION_KIND_GLOBAL) -- this is what  *)
(* makes the transient deactivation below sound for compiled code.          *)
(*                                                                          *)
(* Reader (compiled against the declared type Tc published when compiled):  *)
(*   r1  v := slot                                                          *)
(*   r2  light fence; read flag                                             *)
(*   r3  flag clear:  TRUST v at type Tc  (ReaderSoundness is the theorem)   *)
(*       flag set:    verify typeof(v) <: Tc, error on mismatch (safe)       *)
(*                                                                          *)
(* Re-typer (jl_declare_global, holding world_counter_lock throughout):     *)
(*   t1  decide Tnew; diverged := a *different* typed-GLOBAL epoch exists    *)
(*       in the partition history; transition := the currently published    *)
(*       restriction is writable and differs from Tnew (and Tnew # Any)      *)
(*   t2  if diverged \/ transition: activated := (flag was clear);           *)
(*       flag := 1; if activated: heavy fence (membarrier), then DRAIN:      *)
(*       wait until every thread's commit window is closed                   *)
(*   t3  bare form (`global x::T`, no value): validate that the retained     *)
(*       slot value conforms to Tnew; if not, roll back (clear the flag iff  *)
(*       this call activated it) and error -- nothing is published           *)
(*   t4  install the new restriction (unpublished); if value-carrying,       *)
(*       slot := newval (newval conforms to Tnew)                            *)
(*   t5  publish: world := world + 1                                         *)
(*   t6  transient deactivation: if activated /\ ~diverged /\ published:     *)
(*       clear the flag (release)                                            *)
(*                                                                          *)
(* -------------------- Abstraction of memory ordering -------------------- *)
(*                                                                          *)
(* Unlike the sibling spec SchedulerWake.tla, this protocol's correctness   *)
(* DOES rest on memory ordering, so we do not assume full sequential        *)
(* consistency for the flag. The membarrier's asymmetric-fence guarantee is *)
(* modeled explicitly:                                                      *)
(*                                                                          *)
(*  1. STALE FLAG READS. The activation is split into phases: after         *)
(*     flag := 1 but before the membarrier completes (`fenced` = FALSE), a  *)
(*     storer's s4 / reader's r2 flag read may nondeterministically return  *)
(*     the stale value (clear). After the membarrier action (`fenced` =     *)
(*     TRUE), all flag reads return the true value. This captures "a flag   *)
(*     check that executed before the IPI may miss the flag; one that       *)
(*     executed after it cannot".                                           *)
(*                                                                          *)
(*  2. WINDOW ANNOUNCEMENTS ARE IMMEDIATELY VISIBLE. `window` is a plain    *)
(*     shared variable here. Justification: the announcement (s3) precedes  *)
(*     the flag read (s4) in program order, separated by a compiler-only    *)
(*     fence (jl_signal_fence / the Monotonic store + fenced load pair in   *)
(*     emit_binding_commit_begin). If the flag read executed before the     *)
(*     IPI -- the only case in which the storer proceeds despite an          *)
(*     in-progress activation -- the IPI's full barrier flushes the          *)
(*     announcement before jl_membarrier returns, hence before the drain    *)
(*     begins. A storer whose flag read happens after the IPI reads the     *)
(*     true flag and diverts, so its window state is irrelevant.            *)
(*                                                                          *)
(*  3. DRAIN ACQUIRE / CLOSE RELEASE. Window close (s6) is a release store  *)
(*     paired with the drain's acquire loads: everything inside the window  *)
(*     (the commit) is visible to the re-typer once the drain observes the  *)
(*     window closed. Accordingly the commit (s5) takes effect on the       *)
(*     shared `slot` strictly before the window closes, and the drain       *)
(*     action is enabled only when every window is closed.                  *)
(*                                                                          *)
(*  4. TRANSIENT CLEAR RELEASE / s4 ACQUIRE. The t6 clear is a release; the *)
(*     s4 flag load is an acquire. If s4 observes the CLEARED flag it also  *)
(*     observes the world published at t5. In this model t5 precedes t6     *)
(*     atomically under the lock and TLC state is global, so this pairing   *)
(*     is automatic -- but note that the stale-read nondeterminism of rule   *)
(*     1 applies only to the FLAG, never to the world value read in s4: a   *)
(*     stale-clear flag read can only occur between an activation and its   *)
(*     membarrier, during which the activating re-declaration has not yet   *)
(*     published anything, so the world a racing s4 needs to compare        *)
(*     against is not concurrently moving in a way the staleness could      *)
(*     hide. The dangerous direction -- flag reads clear because it was      *)
(*     transiently CLEARED (t6) while the world already moved (t5) -- is     *)
(*     exactly what the release/acquire pairing (and this model) makes       *)
(*     accurate.                                                            *)
(*                                                                          *)
(*  5. MERGED INSTALL + VALUE SWAP + PUBLICATION (t4 + t5). In the C code   *)
(*     the value store and the world bump are two release stores. We model  *)
(*     them as one atomic action. Justification: the installed-but-         *)
(*     unpublished partition is invisible to other threads (they resolve    *)
(*     restrictions through the published world); the pre-publication slot  *)
(*     swap IS observable through the slot, but only by threads whose       *)
(*     trust in the slot's type is gated by the flag -- which is set and    *)
(*     fenced throughout (a value-carrying swap of a *different* type       *)
(*     implies diverged \/ transition, hence guards active and drained      *)
(*     before t4; a same-type swap cannot violate any restriction). Every   *)
(*     observer therefore either verifies (safe either way) or diverts to   *)
(*     the lock we hold, so no trusted access can distinguish the merged    *)
(*     step from the two-store original. Merging also makes TypeSlotOK      *)
(*     expressible in its strong, unconditional form.                       *)
(*                                                                          *)
(* Other simplifications (honesty section):                                 *)
(*   - One binding, one storer of each kind, one reader; scripts bound the  *)
(*     state space. The re-typer performs a fixed script of declarations.   *)
(*   - Store buffering of the slot itself is not modeled: slot loads/stores *)
(*     are atomic (they are seq_cst/release atomics in the C code, and the  *)
(*     protocol's correctness argument never relies on slot-store           *)
(*     reordering, only on the flag/window/world orderings modeled above).  *)
(*   - Stale reads of a *cleared* flag as still-set are not modeled: that   *)
(*     direction only sends a storer/reader to the locked/verify path,      *)
(*     which is always safe (it can delay, never unsound).                  *)
(*   - The locked store path (revalidate against latest + store under       *)
(*     world_counter_lock) is one atomic action: the lock excludes the only *)
(*     writers of the restriction, and interleaved fast-path commits of     *)
(*     concurrently-validated values commute with it w.r.t. our invariants. *)
(*   - A failed bare re-declaration leaves its installed partition          *)
(*     unpublished; the real code reuses/overwrites it in place on the next *)
(*     declaration (min_world == new_world under the still-held pattern of  *)
(*     world usage here), so the model simply discards it. Keeping it would *)
(*     only ever make `diverged` spuriously TRUE, which is conservative.    *)
(*   - jl_delete_binding and const-over-global re-declarations are omitted: *)
(*     they are t2 with a sticky flag (plus, for const, a slot store before *)
(*     publication) and no transient deactivation, i.e. strictly less racy  *)
(*     than the diverged value-carrying declaration modeled here.           *)
(*   - The undefined (NULL) slot state is not modeled; the slot always      *)
(*     holds a value. Undef-ness is orthogonal to the type-safety protocol. *)
(*                                                                          *)
(* Two constants inject known bugs so the model can demonstrate it has      *)
(* teeth (see MCBuggyNoWorldCheck / MCBuggyEarlyValidation):                *)
(*   - StorerWorldCheck = FALSE removes the world comparison from the       *)
(*     runtime storer's s4, re-creating the unsoundness that the transient  *)
(*     deactivation (t6) would otherwise cause.                             *)
(*   - EarlyBareValidation = TRUE performs the bare re-declaration's        *)
(*     retained-value validation BEFORE guard activation and the drain      *)
(*     (the ordering bug found during review of the PR), instead of after.  *)
(***************************************************************************)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS
    Script,              \* sequence of declarations [ty |-> Type, val |-> Value or "none"]
    StoreAttempts,       \* store attempts per storer thread
    ReadAttempts,        \* reads performed by the compiled reader
    StorerWorldCheck,    \* TRUE: faithful model. FALSE: drop s4's world check (bug)
    EarlyBareValidation  \* FALSE: faithful model. TRUE: t3 validates before t2 (bug)

(* A tiny type lattice: Int and Float are disjoint subtypes of Any. Value    *)
(* "i" has runtime type Int, value "f" has runtime type Float.               *)
Types      == {"Any", "Int", "Float"}
Values     == {"i", "f"}
NoVal      == "none"
TypeofVal(v)  == IF v = "i" THEN "Int" ELSE "Float"
Conforms(v, T) == T = "Any" \/ TypeofVal(v) = T

StorerIds == {"rs", "cc"}   \* runtime-path storer, compiled-path storer

ASSUME /\ StoreAttempts \in Nat /\ ReadAttempts \in Nat
       /\ StorerWorldCheck \in BOOLEAN /\ EarlyBareValidation \in BOOLEAN
       /\ \A i \in 1..Len(Script) :
            /\ Script[i].ty \in Types
            /\ Script[i].val \in Values \cup {NoVal}
            \* a carried value conforms to the carried type by construction
            \* (jl_declare_global validates it before touching any state)
            /\ Script[i].val # NoVal => Conforms(Script[i].val, Script[i].ty)

VARIABLES
    world,      \* published world counter
    pubKind,    \* "Declared" (weak `global x`, restriction Any) or "Global" (typed)
    pubTy,      \* the published restriction (meaningful when pubKind = "Global")
    slot,       \* the binding's single value slot
    flag,       \* BINDING_FLAG_RETYPED (architecturally true value)
    fenced,     \* FALSE only between an activation's flag := 1 and its membarrier
    ghist,      \* set of restrictions of all typed-GLOBAL epochs ever published
    lockHeld,   \* world_counter_lock (held only by the re-typer; locked stores are atomic)
    window,     \* window[s]: storer s's ptls->bnd_commit_window
    badStore,   \* history flag: some commit stored a value not conforming to the
                \* restriction published at commit time (StoreValidation's witness)
    rs,         \* runtime-path storer:  [pc, left, val, vw]
    cc,         \* compiled-path storer: [pc, left, val, tc]
    rd,         \* compiled reader:      [pc, left, val, flg, tc]
    rt          \* re-typer:             [pc, idx, tnew, nval, bare, div, trans,
                \*                        act, repl, savedW, savedF, earlyOK]

vars == <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld, window,
          badStore, rs, cc, rd, rt>>

(* The restriction that governs stores in the currently published world.    *)
Eff == IF pubKind = "Declared" THEN "Any" ELSE pubTy

(* Rule 1: a flag read is accurate except between an activation and its     *)
(* membarrier, where it may also return the stale (clear) value.            *)
FlagReads == IF flag /\ ~fenced THEN {TRUE, FALSE} ELSE {flag}

TypeOK ==
    /\ world \in 0..Len(Script)
    /\ pubKind \in {"Declared", "Global"}
    /\ pubTy \in Types
    /\ slot \in Values
    /\ flag \in BOOLEAN /\ fenced \in BOOLEAN
    /\ ghist \subseteq Types
    /\ lockHeld \in BOOLEAN /\ badStore \in BOOLEAN
    /\ window \in [StorerIds -> BOOLEAN]
    /\ rs \in [pc: {"idle", "ready", "open", "commit", "close", "locked"},
               left: 0..StoreAttempts, val: Values \cup {NoVal},
               vw: 0..Len(Script)]
    /\ cc \in [pc: {"idle", "ready", "open", "commit", "close", "locked"},
               left: 0..StoreAttempts, val: Values \cup {NoVal},
               tc: Types \cup {NoVal}]
    /\ rd \in [pc: {"idle", "r2", "r3"}, left: 0..ReadAttempts,
               val: Values \cup {NoVal}, flg: BOOLEAN, tc: Types \cup {NoVal}]
    /\ rt \in [pc: {"idle", "activate", "fence", "drain", "validate",
                    "publish", "finish", "failed"},
               idx: 1..(Len(Script) + 1), tnew: Types \cup {NoVal},
               nval: Values \cup {NoVal}, bare: BOOLEAN, div: BOOLEAN,
               trans: BOOLEAN, act: BOOLEAN, repl: BOOLEAN,
               savedW: 0..Len(Script), savedF: BOOLEAN, earlyOK: BOOLEAN]

Init ==
    /\ world = 0
    /\ pubKind = "Declared"        \* weak `global x`: stores validate against Any
    /\ pubTy = "Any"
    /\ slot = "i"
    /\ flag = FALSE /\ fenced = TRUE
    /\ ghist = {}
    /\ lockHeld = FALSE /\ badStore = FALSE
    /\ window = [s \in StorerIds |-> FALSE]
    /\ rs = [pc |-> "idle", left |-> StoreAttempts, val |-> NoVal, vw |-> 0]
    /\ cc = [pc |-> "idle", left |-> StoreAttempts, val |-> NoVal, tc |-> NoVal]
    /\ rd = [pc |-> "idle", left |-> ReadAttempts, val |-> NoVal,
             flg |-> FALSE, tc |-> NoVal]
    /\ rt = [pc |-> "idle", idx |-> 1, tnew |-> NoVal, nval |-> NoVal,
             bare |-> FALSE, div |-> FALSE, trans |-> FALSE, act |-> FALSE,
             repl |-> FALSE, savedW |-> 0, savedF |-> FALSE, earlyOK |-> TRUE]

-----------------------------------------------------------------------------
(* Runtime-path storer (jl_checked_assignment).                              *)

(* s1 + s2: capture the validated world and validate a nondeterministically  *)
(* chosen value against the restriction it publishes. A non-conforming value *)
(* raises a type error: the attempt is abandoned. (See the header for why    *)
(* folding the two world loads of the C code into one step is faithful.)     *)
RS_Validate ==
    /\ rs.pc = "idle" /\ rs.left > 0
    /\ \E v \in Values :
         rs' = IF Conforms(v, Eff)
               THEN [rs EXCEPT !.pc = "ready", !.val = v, !.vw = world]
               ELSE [rs EXCEPT !.left = @ - 1]    \* type error at s2
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   window, badStore, cc, rd, rt>>

(* s3: announce the commit window (rule 2: immediately visible).             *)
RS_Open ==
    /\ rs.pc = "ready"
    /\ window' = [window EXCEPT !["rs"] = TRUE]
    /\ rs' = [rs EXCEPT !.pc = "open"]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   badStore, cc, rd, rt>>

(* s4: re-check the flag (possibly stale, rule 1) and -- on the faithful      *)
(* path -- the world. Either divert to the locked path (window closed) or     *)
(* proceed to the commit with the window still open.                         *)
RS_Check ==
    /\ rs.pc = "open"
    /\ \E fr \in FlagReads :
         IF fr \/ (StorerWorldCheck /\ world # rs.vw)
         THEN /\ window' = [window EXCEPT !["rs"] = FALSE]
              /\ rs' = [rs EXCEPT !.pc = "locked"]
         ELSE /\ rs' = [rs EXCEPT !.pc = "commit"]
              /\ UNCHANGED window
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   badStore, cc, rd, rt>>

(* s5: the commit, inside the still-open window (rule 3). Record for         *)
(* StoreValidation whether the committed value conforms to the restriction   *)
(* published *now*.                                                          *)
RS_Commit ==
    /\ rs.pc = "commit"
    /\ slot' = rs.val
    /\ badStore' = (badStore \/ ~Conforms(rs.val, Eff))
    /\ rs' = [rs EXCEPT !.pc = "close"]
    /\ UNCHANGED <<world, pubKind, pubTy, flag, fenced, ghist, lockHeld,
                   window, cc, rd, rt>>

(* s6: close the window (release, rule 3).                                   *)
RS_Close ==
    /\ rs.pc = "close"
    /\ window' = [window EXCEPT !["rs"] = FALSE]
    /\ rs' = [rs EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   badStore, cc, rd, rt>>

(* The locked path: under world_counter_lock, re-validate against the        *)
(* latest published restriction and store; a non-conforming value errors.    *)
(* One atomic action -- see the header's honesty section.                     *)
RS_Locked ==
    /\ rs.pc = "locked" /\ ~lockHeld
    /\ slot' = IF Conforms(rs.val, Eff) THEN rs.val ELSE slot
    /\ rs' = [rs EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<world, pubKind, pubTy, flag, fenced, ghist, lockHeld,
                   window, badStore, cc, rd, rt>>

-----------------------------------------------------------------------------
(* Compiled fast-path storer (typed_store's commit window in cgutils.cpp).   *)
(* Compiled code exists only for typed-GLOBAL epochs, and its stored value   *)
(* conforms to the declared type Tc it was compiled against (the typecheck   *)
(* is compiled ahead of the window). Its s4 checks the FLAG ONLY.            *)

CC_Compile ==
    /\ cc.tc = NoVal /\ pubKind = "Global"
    /\ cc' = [cc EXCEPT !.tc = pubTy]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   window, badStore, rs, rd, rt>>

CC_Prepare ==
    /\ cc.pc = "idle" /\ cc.left > 0 /\ cc.tc # NoVal
    /\ \E v \in {w \in Values : Conforms(w, cc.tc)} :
         cc' = [cc EXCEPT !.pc = "ready", !.val = v]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   window, badStore, rs, rd, rt>>

CC_Open ==
    /\ cc.pc = "ready"
    /\ window' = [window EXCEPT !["cc"] = TRUE]
    /\ cc' = [cc EXCEPT !.pc = "open"]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   badStore, rs, rd, rt>>

(* Flag-only re-check: sound because compiled fast paths exist only for      *)
(* typed-GLOBAL epochs, and a transient deactivation (t6) only happens when  *)
(* no divergent typed-GLOBAL epoch exists -- i.e. every compiled fast path    *)
(* that exists was compiled against the very type still published.           *)
CC_Check ==
    /\ cc.pc = "open"
    /\ \E fr \in FlagReads :
         IF fr
         THEN /\ window' = [window EXCEPT !["cc"] = FALSE]
              /\ cc' = [cc EXCEPT !.pc = "locked"]
         ELSE /\ cc' = [cc EXCEPT !.pc = "commit"]
              /\ UNCHANGED window
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   badStore, rs, rd, rt>>

CC_Commit ==
    /\ cc.pc = "commit"
    /\ slot' = cc.val
    /\ badStore' = (badStore \/ ~Conforms(cc.val, Eff))
    /\ cc' = [cc EXCEPT !.pc = "close"]
    /\ UNCHANGED <<world, pubKind, pubTy, flag, fenced, ghist, lockHeld,
                   window, rs, rd, rt>>

CC_Close ==
    /\ cc.pc = "close"
    /\ window' = [window EXCEPT !["cc"] = FALSE]
    /\ cc' = [cc EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   badStore, rs, rd, rt>>

(* The compiled deopt path re-enters the runtime storer, whose worst-case    *)
(* (and, for our invariants, representative) execution is the locked path.   *)
CC_Locked ==
    /\ cc.pc = "locked" /\ ~lockHeld
    /\ slot' = IF Conforms(cc.val, Eff) THEN cc.val ELSE slot
    /\ cc' = [cc EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<world, pubKind, pubTy, flag, fenced, ghist, lockHeld,
                   window, badStore, rs, rd, rt>>

-----------------------------------------------------------------------------
(* Compiled reader (emit_globalref): r1 loads the slot, r2 reads the flag    *)
(* program-order later (the light fence keeps them ordered; they are two     *)
(* separate steps here so a re-declaration can interleave), r3 decides.      *)

RD_Compile ==
    /\ rd.tc = NoVal /\ pubKind = "Global"
    /\ rd' = [rd EXCEPT !.tc = pubTy]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   window, badStore, rs, cc, rt>>

RD_Read1 ==
    /\ rd.pc = "idle" /\ rd.left > 0 /\ rd.tc # NoVal
    /\ rd' = [rd EXCEPT !.pc = "r2", !.val = slot]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   window, badStore, rs, cc, rt>>

RD_Read2 ==
    /\ rd.pc = "r2"
    /\ \E fr \in FlagReads : rd' = [rd EXCEPT !.pc = "r3", !.flg = fr]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   window, badStore, rs, cc, rt>>

(* r3: with a clear flag observation the value is TRUSTED at type Tc         *)
(* (ReaderSoundness is checked in this state); with a set flag it is         *)
(* verified, which either passes or errors -- safe either way.               *)
RD_Resolve ==
    /\ rd.pc = "r3"
    /\ rd' = [rd EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   window, badStore, rs, cc, rt>>

-----------------------------------------------------------------------------
(* Re-typer: jl_declare_global, one declaration per Script entry, holding    *)
(* world_counter_lock from RT_Begin until RT_Finish / RT_Fail.               *)

(* t1: take the lock; compute `diverged` (a different typed-GLOBAL epoch in  *)
(* history), `transition` (published restriction writable -- always, here --  *)
(* and different from Tnew # Any) and `replaced` (a new partition must be    *)
(* installed and published). Under EarlyBareValidation the retained-value    *)
(* verdict is -- wrongly -- frozen here, before activation and drain.          *)
RT_Begin ==
    /\ rt.pc = "idle" /\ rt.idx <= Len(Script) /\ ~lockHeld
    /\ LET d     == Script[rt.idx]
           tnew  == d.ty
           bare  == d.val = NoVal
           div   == \E T \in ghist : T # tnew
           trans == tnew # "Any" /\ Eff # tnew
           repl  == pubKind = "Declared" \/ pubTy # tnew
       IN /\ lockHeld' = TRUE
          /\ rt' = [rt EXCEPT
                     !.pc     = IF div \/ trans THEN "activate" ELSE "publish",
                     !.tnew   = tnew, !.nval = d.val, !.bare = bare,
                     !.div    = div,  !.trans = trans, !.repl = repl,
                     !.act    = FALSE, !.savedW = world, !.savedF = flag,
                     !.earlyOK = IF EarlyBareValidation /\ bare /\ tnew # "Any"
                                 THEN Conforms(slot, tnew)
                                 ELSE TRUE]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, window,
                   badStore, rs, cc, rd>>

(* t2 (first half): set the flag. On the 0 -> 1 transition the staleness     *)
(* window opens (fenced := FALSE) until the membarrier below; when the flag  *)
(* was already set, the earlier activation's membarrier and drain still      *)
(* cover us (the flag was never cleared since), so both are skipped --        *)
(* exactly as jl_activate_retype_guards returns 0 without fencing.           *)
RT_Activate ==
    /\ rt.pc = "activate"
    /\ LET act == ~flag
       IN /\ flag' = TRUE
          /\ fenced' = IF act THEN FALSE ELSE fenced
          /\ rt' = [rt EXCEPT !.pc = IF act THEN "fence" ELSE "validate",
                              !.act = act]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, ghist, lockHeld, window,
                   badStore, rs, cc, rd>>

(* t2 (membarrier): after this step every flag read is accurate (rule 1).    *)
RT_Fence ==
    /\ rt.pc = "fence"
    /\ fenced' = TRUE
    /\ rt' = [rt EXCEPT !.pc = "drain"]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, ghist, lockHeld, window,
                   badStore, rs, cc, rd>>

(* t2 (drain): wait until every commit window is closed. Windows are         *)
(* straight-line, safepoint-free and never block, so this always terminates  *)
(* (checked via deadlock freedom / Termination).                             *)
RT_Drain ==
    /\ rt.pc = "drain"
    /\ \A s \in StorerIds : ~window[s]
    /\ rt' = [rt EXCEPT !.pc = "validate"]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, lockHeld,
                   window, badStore, rs, cc, rd>>

(* t3: bare re-declarations validate the retained slot value against Tnew    *)
(* -- after the drain, so no store validated against the superseded           *)
(* restriction can still land (the faithful model reads `slot` HERE; the     *)
(* EarlyBareValidation bug uses the verdict frozen at RT_Begin). On failure, *)
(* roll back: clear the flag iff this declaration activated it, publish      *)
(* nothing (BareRetypeSound checks the rollback in the "failed" state).      *)
RT_Validate ==
    /\ rt.pc = "validate"
    /\ IF rt.bare /\ rt.tnew # "Any"
       THEN IF (IF EarlyBareValidation THEN rt.earlyOK
                                       ELSE Conforms(slot, rt.tnew))
            THEN /\ rt' = [rt EXCEPT !.pc = "publish"]
                 /\ UNCHANGED flag
            ELSE /\ flag' = IF rt.act THEN FALSE ELSE flag
                 /\ rt' = [rt EXCEPT !.pc = "failed"]
       ELSE /\ rt' = [rt EXCEPT !.pc = "publish"]
            /\ UNCHANGED flag
    /\ UNCHANGED <<world, pubKind, pubTy, slot, fenced, ghist, lockHeld,
                   window, badStore, rs, cc, rd>>

(* t4 + t5, merged (see header rule 5): install the new restriction, swap in *)
(* a carried value, and publish the new world as one visible event.          *)
RT_Publish ==
    /\ rt.pc = "publish"
    /\ IF rt.repl
       THEN /\ pubKind' = "Global"
            /\ pubTy' = rt.tnew
            /\ ghist' = ghist \cup {rt.tnew}
            /\ world' = world + 1
       ELSE UNCHANGED <<pubKind, pubTy, ghist, world>>
    /\ slot' = IF rt.bare THEN slot ELSE rt.nval
    /\ rt' = [rt EXCEPT !.pc = "finish"]
    /\ UNCHANGED <<flag, fenced, lockHeld, window, badStore, rs, cc, rd>>

(* t6: transient deactivation -- only when this declaration activated the    *)
(* guards, no divergent typed-GLOBAL epoch exists, and a new world was       *)
(* published (whose world check diverts any straggler that validated         *)
(* against the superseded restriction). Then release the lock.               *)
RT_Finish ==
    /\ rt.pc = "finish"
    /\ flag' = IF rt.act /\ ~rt.div /\ rt.repl THEN FALSE ELSE flag
    /\ lockHeld' = FALSE
    /\ rt' = [rt EXCEPT !.pc = "idle", !.idx = @ + 1]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, fenced, ghist, window,
                   badStore, rs, cc, rd>>

(* Error return of a failed bare re-declaration: release the lock and move   *)
(* on (the enclosing program is assumed to catch the error).                 *)
RT_Fail ==
    /\ rt.pc = "failed"
    /\ lockHeld' = FALSE
    /\ rt' = [rt EXCEPT !.pc = "idle", !.idx = @ + 1]
    /\ UNCHANGED <<world, pubKind, pubTy, slot, flag, fenced, ghist, window,
                   badStore, rs, cc, rd>>

-----------------------------------------------------------------------------

Done ==
    /\ rt.pc = "idle" /\ rt.idx > Len(Script)
    /\ rs.pc = "idle" /\ rs.left = 0
    /\ cc.pc = "idle" /\ cc.left = 0
    /\ rd.pc = "idle" /\ rd.left = 0

Next ==
    \/ RS_Validate \/ RS_Open \/ RS_Check \/ RS_Commit \/ RS_Close \/ RS_Locked
    \/ CC_Compile \/ CC_Prepare \/ CC_Open \/ CC_Check \/ CC_Commit
    \/ CC_Close \/ CC_Locked
    \/ RD_Compile \/ RD_Read1 \/ RD_Read2 \/ RD_Resolve
    \/ RT_Begin \/ RT_Activate \/ RT_Fence \/ RT_Drain \/ RT_Validate
    \/ RT_Publish \/ RT_Finish \/ RT_Fail
    \* Once every thread has run out of script the system is legitimately
    \* quiescent; allow a stutter step there so TLC's deadlock detector only
    \* fires on genuinely stuck states (e.g. a drain that can never complete).
    \/ (Done /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

-----------------------------------------------------------------------------
(* Properties.                                                               *)

(* 1. The slot's value always conforms to the currently published            *)
(*    restriction. (Expressible unconditionally thanks to the merged         *)
(*    swap+publish step -- see header rule 5.)                                *)
TypeSlotOK == Conforms(slot, Eff)

(* 2. Whenever the reader reaches r3 having observed a CLEAR flag, the value *)
(*    it read at r1 conforms to the declared type it was compiled against.   *)
ReaderSoundness == (rd.pc = "r3" /\ ~rd.flg) => Conforms(rd.val, rd.tc)

(* 3. No fast-path commit ever stored a value that did not conform, at       *)
(*    commit time, to the then-published restriction (equivalently: no store *)
(*    validated against a superseded restriction lands after the superseding *)
(*    publication). The locked path re-validates in the same atomic step, so *)
(*    only fast-path commits can set the witness.                            *)
StoreValidation == ~badStore

(* 4. A failed bare re-declaration publishes nothing and restores the flag   *)
(*    to its pre-declaration state; a successful one leaves a conforming     *)
(*    retained value published (the second conjunct is subsumed by           *)
(*    TypeSlotOK, but stated here as the named theorem).                     *)
BareRetypeSound ==
    /\ rt.pc = "failed" => (world = rt.savedW /\ flag = rt.savedF)
    /\ (rt.pc = "finish" /\ rt.bare /\ rt.repl) => Conforms(slot, pubTy)

(* Liveness sanity check (with WF): every fair behavior finishes all         *)
(* scripts, in particular every drain terminates.                            *)
Termination == <>[]Done

=============================================================================
