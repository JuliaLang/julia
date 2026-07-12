--------------------------- MODULE BindingRetype ---------------------------
(***************************************************************************)
(* A TLA+ model of Julia's global-binding re-typing protocol                *)
(* (issue #62154, PR #62335): the race between stores to a global           *)
(* binding's single value slot, compiled fast-path reads of that slot, and  *)
(* a re-declaration (`global x::T [= v]`) that replaces the binding's       *)
(* declared type while such accesses are in flight.                         *)
(*                                                                          *)
(* The code being modeled:                                                  *)
(*   - src/julia.h: PARTITION_FLAG_RETYPE_READ / PARTITION_FLAG_RETYPE_WRITE *)
(*   - src/julia_internal.h: jl_binding_begin_commit / jl_binding_end_commit *)
(*   - src/module.c: jl_retype_flag_partitions, jl_checked_assignment (and   *)
(*     the other jl_checked_* storers)                                       *)
(*   - src/toplevel.c: jl_declare_global (guard flagging, retained-value     *)
(*     validation, value swap, world publication)                           *)
(*   - src/cgutils.cpp: emit_retype_recheck, emit_binding_commit_begin/end   *)
(*   - src/codegen.cpp: emit_retype_guard, emit_globalref                    *)
(*                                                                          *)
(* Shared state per binding: a value `slot` (each value has a runtime       *)
(* type) and a sequence of *epochs* (binding partitions), each carrying its  *)
(* declared type ("restriction") and two monotone guard flags:              *)
(*                                                                          *)
(*   r (RETYPE_READ):  some later declaration allows the slot to hold a     *)
(*                     value outside this epoch's restriction; reads         *)
(*                     compiled against the epoch must verify what they      *)
(*                     load.                                                 *)
(*   w (RETYPE_WRITE): a value conforming to this epoch's restriction is    *)
(*                     not necessarily storable under some later declared    *)
(*                     type; stores validated against the epoch must divert  *)
(*                     to the locked path.                                   *)
(*                                                                          *)
(* A re-declaration to type Tnew sets, on every prior epoch P (under         *)
(* world_counter_lock, before anything is published):                        *)
(*     P.r  when  ~(Tnew <: P.ty)      (new values may not conform to P)     *)
(*     P.w  when  ~(P.ty <: Tnew)      (P-validated values may violate Tnew) *)
(* then issues the asymmetric heavy fence if any flag transitioned, drains   *)
(* the per-thread commit windows if any *write* flag transitioned,           *)
(* validates the retained value (bare form), and only then swaps in the new  *)
(* value and publishes the new epoch -- whose own flags start clear, so code *)
(* compiled against the new declared type runs unguarded. A pure widening    *)
(* sets no write flags: stale stores keep committing directly, soundly.      *)
(*                                                                          *)
(* The threads:                                                              *)
(*                                                                          *)
(* Runtime storer (jl_checked_assignment):                                   *)
(*   s1  validate a value v against the restriction of the currently         *)
(*       published epoch V (one atomic step; the thread may stall            *)
(*       arbitrarily long afterwards)                                        *)
(*   s3  open the commit window (ptls->bnd_commit_window := 1)               *)
(*   s4  light fence; read V.w (possibly stale, see the fence abstraction):  *)
(*       set ->                                                              *)
(*       close the window and take the LOCKED path (re-validate against      *)
(*       the *latest* published restriction under world_counter_lock,        *)
(*       then store); clear -> proceed                                       *)
(*   s5  commit: slot := v            (window still open)                    *)
(*   s6  close the window (release; pairs with the drain's acquire)          *)
(*                                                                          *)
(* Compiled fast-path storer: same shape; its validation is the typecheck    *)
(* against the declared type Tc of the epoch C it was compiled for (only     *)
(* typed-GLOBAL epochs have compiled fast paths), and s4 checks C.w.         *)
(*                                                                          *)
(* Reader (compiled against epoch R):                                        *)
(*   r1  v := slot                                                           *)
(*   r2  light fence; read R.r (possibly stale)                             *)
(*   r3  clear: TRUST v at R.ty (ReaderSoundness is the theorem)             *)
(*       set:   verify typeof(v) <: R.ty, error on mismatch (always safe)    *)
(*                                                                          *)
(* -------------------- Abstraction of memory ordering -------------------- *)
(*                                                                          *)
(* The protocol's correctness rests on the asymmetric fence, modeled          *)
(* explicitly. Two shared bits are read across threads -- the guard flags    *)
(* (retyper writes, storers/readers read) and the commit-window announcement *)
(* (storer writes, retyper's drain reads) -- and NEITHER is assumed          *)
(* instantaneously visible: in both directions a read may return the stale   *)
(* value until the asymmetric fence, which is exactly what makes it visible. *)
(* The two are modeled the same way, a "pending" set drained by the fence,   *)
(* and a buggy variant that drops either flush shows TLC catches it.         *)
(*                                                                          *)
(*  1. STALE FLAG READS (flag: retyper -> storer). Flags set by the          *)
(*     in-progress declaration are collected in `pendR`/`pendW` until its    *)
(*     membarrier: a read of a pending flag may nondeterministically return  *)
(*     the stale (clear) value; after RT_Fence every read is accurate.       *)
(*     Flags set by earlier (already fenced) declarations are always read    *)
(*     accurately. This captures "a flag check that executed before the IPI  *)
(*     may miss the flag; one that executed after it cannot". The flags are  *)
(*     monotone -- there is no transient clearing in this design -- so no    *)
(*     release/acquire pairing on the flag itself is needed (the world       *)
(*     check of the previous design is gone: a stale storer that validated   *)
(*     against a superseded epoch is diverted by that epoch's sticky write   *)
(*     flag, forever).                                                       *)
(*                                                                          *)
(*  2. STALE WINDOW READS (window: storer -> retyper). Symmetric to rule 1.  *)
(*     A storer's window OPEN (s3) is a plain store behind only a light      *)
(*     compiler fence -- it orders s3 before s4, nothing more -- so until a  *)
(*     barrier flushes it the retyper's drain may read the window either     *)
(*     way. Opens not yet forced globally visible are collected in           *)
(*     `pendWin`; the drain's read ReadWindow(s) of such a storer returns    *)
(*     the stale (closed) value OR the open one, exactly as ReadW does for a *)
(*     pending flag -- so a store buffer that happens to have drained early  *)
(*     is covered by the same nondeterminism, no separate step needed. The   *)
(*     heavy fence (RT_Fence, FenceFlushesWindows) empties pendWin, its IPI  *)
(*     draining each storer's store buffer, after which the drain reads      *)
(*     every open accurately. This is why the fence is required and why it   *)
(*     suffices: a storer that will commit read its write flag clear (s4),   *)
(*     so by rule 1 that read preceded this declaration's fence; s3 is       *)
(*     program-order before s4 and cannot sink past the light fence, so at   *)
(*     fence time its open is still pending and the flush forces the drain   *)
(*     to see it. The CLOSE (s6) is a release the drain acquires             *)
(*     (jl_atomic_store_release, not the light-fenced open), so it is        *)
(*     modeled as promptly visible: clearing window[s] and dropping it from  *)
(*     pendWin makes ReadWindow return just {closed}; see rule 3. Setting    *)
(*     FenceFlushesWindows = FALSE keeps opens pending forever, letting the  *)
(*     drain read an in-flight open as closed and proceed past it            *)
(*     (MCBuggyUnflushedWindow).                                             *)
(*                                                                          *)
(*  3. DRAIN ACQUIRE / CLOSE RELEASE. Window close (s6) is a release store   *)
(*     paired with the drain's acquire loads: everything inside the window   *)
(*     (the commit) is visible to the re-declaration once the drain          *)
(*     observes the window closed. Accordingly the commit takes effect on    *)
(*     the shared `slot` strictly before the window closes, and the drain    *)
(*     proceeds only from an observation (ReadWindow) in which every window  *)
(*     is closed.                                                            *)
(*                                                                          *)
(*  4. MERGED INSTALL + VALUE SWAP + PUBLICATION. As in the C code the swap  *)
(*     precedes the world bump; they are modeled as one atomic action        *)
(*     because every observer that could distinguish them is either          *)
(*     verifying (safe) or diverted to the lock the re-typer holds: a swap  *)
(*     of a value outside some epoch's restriction implies that epoch's      *)
(*     read flag was set and fenced beforehand.                             *)
(*                                                                          *)
(* Other simplifications (honesty section):                                  *)
(*   - One binding; one storer of each kind; one reader; a fixed             *)
(*     declaration script bounds the state space.                            *)
(*   - Slot loads/stores are atomic (they are release/seq_cst atomics in     *)
(*     the C code; the correctness argument never relies on slot-store       *)
(*     reordering, only on the flag/window orderings above).                 *)
(*   - Stale reads of a *set* flag as clear after the fence, or spurious     *)
(*     sets, are not modeled (the former cannot happen, the latter only      *)
(*     sends threads to the safe slow path). Likewise a stale read of a      *)
(*     *closed* window as open is not modeled: it is the safe direction (it  *)
(*     would only make the drain spin longer), and the close is a release    *)
(*     the drain acquires.                                                   *)
(*   - The locked store path is one atomic action: the lock excludes the     *)
(*     only writers of the epoch sequence, and interleaved fast-path         *)
(*     commits of concurrently-validated values commute with it w.r.t. our   *)
(*     invariants.                                                           *)
(*   - A failed bare re-declaration publishes nothing; the flags it set      *)
(*     remain (deliberately: they are conservative, and only pessimize       *)
(*     superseded epochs).                                                   *)
(*   - delete_binding and const-over-global are jl_retype_flag_partitions    *)
(*     calls with store_ty = NULL (write-flag every epoch): delete passes    *)
(*     slot_ty = NULL too (so it sets no read flags), const-over-global the  *)
(*     constant's type. Both are strictly less permissive instances of the   *)
(*     modeled transitions and are omitted.                                  *)
(*   - The compiled storer models the Set commit (one write-guard check).    *)
(*     The RMW kinds (swap/replace/modify) additionally re-check the READ    *)
(*     guard -- they trust values they load from the slot, the obligation    *)
(*     the Reader already models -- and the outer early-out guard before the *)
(*     window is a pure optimization over the authoritative in-window        *)
(*     recheck; neither adds behavior the invariants do not already cover.   *)
(*   - The undefined (NULL) slot state is not modeled.                       *)
(*                                                                          *)
(* Three constants inject known-shape bugs so the model can demonstrate it   *)
(* has teeth (MCBuggySwappedFlags / MCBuggyEarlyValidation /                 *)
(* MCBuggyUnflushedWindow):                                                  *)
(*   - SwappedFlagConditions = TRUE computes each flag with the converse     *)
(*     subtype test (reads flagged when ~(P.ty <: Tnew), writes when         *)
(*     ~(Tnew <: P.ty)), which looks plausible and is exactly wrong.         *)
(*   - EarlyBareValidation = TRUE performs the bare re-declaration's         *)
(*     retained-value validation BEFORE the flagging and drain (the          *)
(*     ordering bug found during review of the PR), instead of after.        *)
(*   - FenceFlushesWindows = FALSE makes the heavy fence NOT drain the       *)
(*     pending window opens, so the drain can read an in-flight open as      *)
(*     closed and proceed past the commit -- i.e. it treats the             *)
(*     announcement as visible without the barrier, the assumption this      *)
(*     model deliberately does not make.                                     *)
(***************************************************************************)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS
    Script,               \* sequence of declarations [ty |-> Type, val |-> Value or "none"]
    StoreAttempts,        \* store attempts per storer thread
    ReadAttempts,         \* reads performed by the compiled reader
    SwappedFlagConditions, \* FALSE: faithful model. TRUE: converse subtype tests (bug)
    EarlyBareValidation,  \* FALSE: faithful model. TRUE: t3 validates before t2 (bug)
    FenceFlushesWindows   \* TRUE: faithful model. FALSE: fence omits the window flush (bug)

(* A tiny type lattice: Int and Float are disjoint subtypes of Any. Value    *)
(* "i" has runtime type Int, value "f" has runtime type Float.               *)
Types      == {"Any", "Int", "Float"}
Values     == {"i", "f"}
NoVal      == "none"
TypeofVal(v)  == IF v = "i" THEN "Int" ELSE "Float"
Conforms(v, T) == T = "Any" \/ TypeofVal(v) = T
Subtype(S, T)  == S = T \/ T = "Any"

StorerIds == {"rs", "cc"}   \* runtime-path storer, compiled-path storer

ASSUME /\ StoreAttempts \in Nat /\ ReadAttempts \in Nat
       /\ SwappedFlagConditions \in BOOLEAN /\ EarlyBareValidation \in BOOLEAN
       /\ FenceFlushesWindows \in BOOLEAN
       /\ \A i \in 1..Len(Script) :
            /\ Script[i].ty \in Types
            /\ Script[i].val \in Values \cup {NoVal}
            \* a carried value conforms to the carried type by construction
            /\ Script[i].val # NoVal => Conforms(Script[i].val, Script[i].ty)

MaxEp == Len(Script) + 1   \* epoch 1 is the initial weak (`global x`, Any) epoch

VARIABLES
    eps,        \* Seq of epochs [ty, r, w]; eps[pub] is the published one
    pub,        \* index of the currently published epoch
    slot,       \* the binding's single value slot
    pendR,      \* epoch indices whose r flag was set by the in-progress,
                \* not-yet-fenced declaration (stale-readable, rule 1)
    pendW,      \* likewise for w flags
    lockHeld,   \* world_counter_lock (held only by the re-typer)
    window,     \* window[s]: storer s's ptls->bnd_commit_window (its LOCAL bit)
    pendWin,    \* storers whose window OPEN is not yet forced visible to the drain
                \* (stale-readable as closed until the fence flushes it, rule 2)
    badStore,   \* history flag: some fast-path commit stored a value that did not
                \* conform to the restriction published at commit time
    rs,         \* runtime-path storer:  [pc, left, val, vep]
    cc,         \* compiled-path storer: [pc, left, val, cep]
    rd,         \* compiled reader:      [pc, left, val, flg, cep]
    rt          \* re-typer:             [pc, idx, tnew, nval, bare, drain, earlyOK]

vars == <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin, badStore,
          rs, cc, rd, rt>>

(* Rule 1: a flag read is accurate except while the setting declaration has  *)
(* not yet fenced, where it may also return the stale (clear) value.         *)
ReadR(i) == IF eps[i].r THEN (IF i \in pendR THEN {TRUE, FALSE} ELSE {TRUE})
            ELSE {FALSE}
ReadW(i) == IF eps[i].w THEN (IF i \in pendW THEN {TRUE, FALSE} ELSE {TRUE})
            ELSE {FALSE}

(* Rule 2: the drain's read of a storer's window. An open (window[s]) that    *)
(* has not been forced visible (s \in pendWin) may still read closed; once    *)
(* the fence has flushed it, or it was never pending, it reads accurately.    *)
(* A closed window always reads closed (the close is a release, rule 3).      *)
ReadWindow(s) == IF window[s] THEN (IF s \in pendWin THEN {TRUE, FALSE} ELSE {TRUE})
                 ELSE {FALSE}

(* The faithful (and, under SwappedFlagConditions, the buggy) flag calculus. *)
NeedR(pty, tnew) == IF SwappedFlagConditions THEN ~Subtype(pty, tnew)
                                             ELSE ~Subtype(tnew, pty)
NeedW(pty, tnew) == IF SwappedFlagConditions THEN ~Subtype(tnew, pty)
                                             ELSE ~Subtype(pty, tnew)

TypeOK ==
    /\ eps \in Seq([ty: Types, r: BOOLEAN, w: BOOLEAN])
    /\ Len(eps) \in 1..MaxEp
    /\ pub \in 1..Len(eps)
    /\ slot \in Values
    /\ pendR \subseteq 1..Len(eps) /\ pendW \subseteq 1..Len(eps)
    /\ lockHeld \in BOOLEAN /\ badStore \in BOOLEAN
    /\ window \in [StorerIds -> BOOLEAN]
    /\ pendWin \subseteq StorerIds
    /\ rs \in [pc: {"idle", "ready", "open", "commit", "close", "locked"},
               left: 0..StoreAttempts, val: Values \cup {NoVal}, vep: 1..MaxEp]
    /\ cc \in [pc: {"idle", "ready", "open", "commit", "close", "locked"},
               left: 0..StoreAttempts, val: Values \cup {NoVal}, cep: 0..MaxEp]
    /\ rd \in [pc: {"idle", "r2", "r3"}, left: 0..ReadAttempts,
               val: Values \cup {NoVal}, flg: BOOLEAN, cep: 0..MaxEp]
    /\ rt \in [pc: {"idle", "flag", "fence", "drain", "validate",
                    "publish", "finish", "failed"},
               idx: 1..(Len(Script) + 1), tnew: Types \cup {NoVal},
               nval: Values \cup {NoVal}, bare: BOOLEAN, drain: BOOLEAN,
               earlyOK: BOOLEAN]

Init ==
    /\ eps = << [ty |-> "Any", r |-> FALSE, w |-> FALSE] >>  \* weak `global x`
    /\ pub = 1
    /\ slot = "i"
    /\ pendR = {} /\ pendW = {}
    /\ lockHeld = FALSE /\ badStore = FALSE
    /\ window = [s \in StorerIds |-> FALSE]
    /\ pendWin = {}
    /\ rs = [pc |-> "idle", left |-> StoreAttempts, val |-> NoVal, vep |-> 1]
    /\ cc = [pc |-> "idle", left |-> StoreAttempts, val |-> NoVal, cep |-> 0]
    /\ rd = [pc |-> "idle", left |-> ReadAttempts, val |-> NoVal,
             flg |-> FALSE, cep |-> 0]
    /\ rt = [pc |-> "idle", idx |-> 1, tnew |-> NoVal, nval |-> NoVal,
             bare |-> FALSE, drain |-> FALSE, earlyOK |-> TRUE]

-----------------------------------------------------------------------------
(* Runtime-path storer (jl_checked_assignment).                              *)

(* s1: validate a nondeterministically chosen value against the currently    *)
(* published epoch's restriction, remembering that epoch; a non-conforming   *)
(* value raises a type error (the attempt is abandoned).                     *)
RS_Validate ==
    /\ rs.pc = "idle" /\ rs.left > 0
    /\ \E v \in Values :
         rs' = IF Conforms(v, eps[pub].ty)
               THEN [rs EXCEPT !.pc = "ready", !.val = v, !.vep = pub]
               ELSE [rs EXCEPT !.left = @ - 1]    \* type error at s1
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, cc, rd, rt>>

(* s3: open the commit window. Rule 2: the open is buffered -- the storer's   *)
(* local bit is set and the storer is added to pendWin, so the drain may     *)
(* still read the window closed until a fence flushes it.                     *)
RS_Open ==
    /\ rs.pc = "ready"
    /\ window' = [window EXCEPT !["rs"] = TRUE]
    /\ pendWin' = pendWin \cup {"rs"}
    /\ rs' = [rs EXCEPT !.pc = "open"]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, badStore, cc, rd, rt>>

(* s4: re-check the validated epoch's write flag (possibly stale, rule 1):   *)
(* divert to the locked path (window closed, visibly) or proceed to commit.  *)
RS_Check ==
    /\ rs.pc = "open"
    /\ \E fr \in ReadW(rs.vep) :
         IF fr
         THEN /\ window' = [window EXCEPT !["rs"] = FALSE]
              /\ pendWin' = pendWin \ {"rs"}
              /\ rs' = [rs EXCEPT !.pc = "locked"]
         ELSE /\ rs' = [rs EXCEPT !.pc = "commit"]
              /\ UNCHANGED <<window, pendWin>>
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, badStore, cc, rd, rt>>

(* s5: the commit, inside the still-open window (rule 3).                    *)
RS_Commit ==
    /\ rs.pc = "commit"
    /\ slot' = rs.val
    /\ badStore' = (badStore \/ ~Conforms(rs.val, eps[pub].ty))
    /\ rs' = [rs EXCEPT !.pc = "close"]
    /\ UNCHANGED <<eps, pub, pendR, pendW, lockHeld, window, pendWin, cc, rd, rt>>

(* s6: close the window (release, rules 2/3): the close is visible at once.   *)
RS_Close ==
    /\ rs.pc = "close"
    /\ window' = [window EXCEPT !["rs"] = FALSE]
    /\ pendWin' = pendWin \ {"rs"}
    /\ rs' = [rs EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, badStore, cc, rd, rt>>

(* The locked path: under world_counter_lock, re-validate against the        *)
(* latest published restriction and store; a non-conforming value errors.    *)
RS_Locked ==
    /\ rs.pc = "locked" /\ ~lockHeld
    /\ slot' = IF Conforms(rs.val, eps[pub].ty) THEN rs.val ELSE slot
    /\ rs' = [rs EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<eps, pub, pendR, pendW, lockHeld, window, pendWin, badStore,
                   cc, rd, rt>>

-----------------------------------------------------------------------------
(* Compiled fast-path storer (typed_store's commit window). Compiled code     *)
(* exists only for typed-GLOBAL epochs (index >= 2 here: epoch 1 is the weak *)
(* Declared epoch), and its stored value conforms to the epoch's declared    *)
(* type by the compiled-in typecheck.                                        *)

CC_Compile ==
    /\ cc.cep = 0 /\ pub >= 2
    /\ cc' = [cc EXCEPT !.cep = pub]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, rs, rd, rt>>

CC_Prepare ==
    /\ cc.pc = "idle" /\ cc.left > 0 /\ cc.cep # 0
    /\ \E v \in {w \in Values : Conforms(w, eps[cc.cep].ty)} :
         cc' = [cc EXCEPT !.pc = "ready", !.val = v]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, rs, rd, rt>>

CC_Open ==
    /\ cc.pc = "ready"
    /\ window' = [window EXCEPT !["cc"] = TRUE]
    /\ pendWin' = pendWin \cup {"cc"}
    /\ cc' = [cc EXCEPT !.pc = "open"]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, badStore, rs, rd, rt>>

CC_Check ==
    /\ cc.pc = "open"
    /\ \E fr \in ReadW(cc.cep) :
         IF fr
         THEN /\ window' = [window EXCEPT !["cc"] = FALSE]
              /\ pendWin' = pendWin \ {"cc"}
              /\ cc' = [cc EXCEPT !.pc = "locked"]
         ELSE /\ cc' = [cc EXCEPT !.pc = "commit"]
              /\ UNCHANGED <<window, pendWin>>
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, badStore, rs, rd, rt>>

CC_Commit ==
    /\ cc.pc = "commit"
    /\ slot' = cc.val
    /\ badStore' = (badStore \/ ~Conforms(cc.val, eps[pub].ty))
    /\ cc' = [cc EXCEPT !.pc = "close"]
    /\ UNCHANGED <<eps, pub, pendR, pendW, lockHeld, window, pendWin, rs, rd, rt>>

CC_Close ==
    /\ cc.pc = "close"
    /\ window' = [window EXCEPT !["cc"] = FALSE]
    /\ pendWin' = pendWin \ {"cc"}
    /\ cc' = [cc EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, badStore, rs, rd, rt>>

(* The compiled deopt path re-enters the runtime storer, whose worst-case    *)
(* (and, for our invariants, representative) execution is the locked path.   *)
CC_Locked ==
    /\ cc.pc = "locked" /\ ~lockHeld
    /\ slot' = IF Conforms(cc.val, eps[pub].ty) THEN cc.val ELSE slot
    /\ cc' = [cc EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<eps, pub, pendR, pendW, lockHeld, window, pendWin, badStore,
                   rs, rd, rt>>

-----------------------------------------------------------------------------
(* Compiled reader (emit_globalref): r1 loads the slot, r2 reads its         *)
(* epoch's read flag program-order later (the light fence keeps them         *)
(* ordered; two separate steps so a re-declaration can interleave).          *)

RD_Compile ==
    /\ rd.cep = 0 /\ pub >= 2
    /\ rd' = [rd EXCEPT !.cep = pub]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, rs, cc, rt>>

RD_Read1 ==
    /\ rd.pc = "idle" /\ rd.left > 0 /\ rd.cep # 0
    /\ rd' = [rd EXCEPT !.pc = "r2", !.val = slot]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, rs, cc, rt>>

RD_Read2 ==
    /\ rd.pc = "r2"
    /\ \E fr \in ReadR(rd.cep) : rd' = [rd EXCEPT !.pc = "r3", !.flg = fr]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, rs, cc, rt>>

(* r3: with a clear flag observation the value is TRUSTED at the compiled    *)
(* epoch's type (ReaderSoundness is checked in this state); with a set flag  *)
(* it is verified, which either passes or errors -- safe either way.         *)
RD_Resolve ==
    /\ rd.pc = "r3"
    /\ rd' = [rd EXCEPT !.pc = "idle", !.left = @ - 1]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, rs, cc, rt>>

-----------------------------------------------------------------------------
(* Re-typer: jl_declare_global, one declaration per Script entry, holding    *)
(* world_counter_lock from RT_Begin until RT_Finish / RT_Fail.               *)

RT_Begin ==
    /\ rt.pc = "idle" /\ rt.idx <= Len(Script) /\ ~lockHeld
    /\ LET d == Script[rt.idx]
       IN /\ lockHeld' = TRUE
          /\ rt' = [rt EXCEPT
                     !.pc = "flag", !.tnew = d.ty, !.nval = d.val,
                     !.bare = (d.val = NoVal), !.drain = FALSE,
                     !.earlyOK = IF EarlyBareValidation /\ d.val = NoVal /\ d.ty # "Any"
                                 THEN Conforms(slot, d.ty)
                                 ELSE TRUE]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, window, pendWin, badStore,
                   rs, cc, rd>>

(* jl_retype_flag_partitions: flag every epoch the new declared type          *)
(* invalidates. Flags transitioned here are stale-readable (rule 1) until    *)
(* RT_Fence. No fence/drain is needed when nothing transitions.              *)
RT_Flag ==
    /\ rt.pc = "flag"
    /\ LET newR == {i \in 1..Len(eps) : ~eps[i].r /\ NeedR(eps[i].ty, rt.tnew)}
           newW == {i \in 1..Len(eps) : ~eps[i].w /\ NeedW(eps[i].ty, rt.tnew)}
       IN /\ eps' = [i \in 1..Len(eps) |->
                       [eps[i] EXCEPT !.r = @ \/ i \in newR,
                                      !.w = @ \/ i \in newW]]
          /\ pendR' = newR
          /\ pendW' = newW
          /\ rt' = [rt EXCEPT !.pc = IF newR \cup newW = {} THEN "validate"
                                                            ELSE "fence",
                              !.drain = newW # {}]
    /\ UNCHANGED <<pub, slot, lockHeld, window, pendWin, badStore, rs, cc, rd>>

(* The asymmetric heavy fence (jl_membarrier): after this step every flag    *)
(* read is accurate (rule 1, pend sets emptied) and every buffered window    *)
(* open has been forced visible to the drain (rule 2, pendWin emptied -- the *)
(* IPI flushing each storer's store buffer). FenceFlushesWindows = FALSE     *)
(* drops the latter, modeling the mistaken belief the open needs no barrier. *)
RT_Fence ==
    /\ rt.pc = "fence"
    /\ pendR' = {} /\ pendW' = {}
    /\ pendWin' = IF FenceFlushesWindows THEN {} ELSE pendWin
    /\ rt' = [rt EXCEPT !.pc = IF rt.drain THEN "drain" ELSE "validate"]
    /\ UNCHANGED <<eps, pub, slot, lockHeld, window, badStore, rs, cc, rd>>

(* Drain: proceed only from an observation in which every commit window reads *)
(* closed (ReadWindow); only needed when a write flag transitioned. Once the  *)
(* fence has emptied pendWin these reads are accurate, so this waits out      *)
(* every genuinely-open window; windows never block, so it terminates.       *)
RT_Drain ==
    /\ rt.pc = "drain"
    /\ \A s \in StorerIds : FALSE \in ReadWindow(s)
    /\ rt' = [rt EXCEPT !.pc = "validate"]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, rs, cc, rd>>

(* Bare re-declarations validate the retained slot value against Tnew --     *)
(* after the drain (the faithful model reads `slot` HERE; the                *)
(* EarlyBareValidation bug uses the verdict frozen at RT_Begin). On failure  *)
(* nothing is published; the flags set above remain, conservatively.         *)
RT_Validate ==
    /\ rt.pc = "validate"
    /\ IF rt.bare /\ rt.tnew # "Any"
       THEN IF (IF EarlyBareValidation THEN rt.earlyOK
                                       ELSE Conforms(slot, rt.tnew))
            THEN rt' = [rt EXCEPT !.pc = "publish"]
            ELSE rt' = [rt EXCEPT !.pc = "failed"]
       ELSE rt' = [rt EXCEPT !.pc = "publish"]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, lockHeld, window, pendWin,
                   badStore, rs, cc, rd>>

(* Install the new epoch (its flags start clear: they live outside            *)
(* PARTITION_MASK_FLAG and are never inherited), swap in a carried value,    *)
(* and publish, as one visible event (abstraction rule 4).                   *)
RT_Publish ==
    /\ rt.pc = "publish"
    /\ eps' = Append(eps, [ty |-> rt.tnew, r |-> FALSE, w |-> FALSE])
    /\ pub' = Len(eps) + 1
    /\ slot' = IF rt.bare THEN slot ELSE rt.nval
    /\ rt' = [rt EXCEPT !.pc = "finish"]
    /\ UNCHANGED <<pendR, pendW, lockHeld, window, pendWin, badStore, rs, cc, rd>>

RT_Finish ==
    /\ rt.pc = "finish"
    /\ lockHeld' = FALSE
    /\ rt' = [rt EXCEPT !.pc = "idle", !.idx = @ + 1]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, window, pendWin, badStore,
                   rs, cc, rd>>

RT_Fail ==
    /\ rt.pc = "failed"
    /\ lockHeld' = FALSE
    /\ rt' = [rt EXCEPT !.pc = "idle", !.idx = @ + 1]
    /\ UNCHANGED <<eps, pub, slot, pendR, pendW, window, pendWin, badStore,
                   rs, cc, rd>>

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
    \/ RT_Begin \/ RT_Flag \/ RT_Fence \/ RT_Drain \/ RT_Validate
    \/ RT_Publish \/ RT_Finish \/ RT_Fail
    \* Once every thread has run out of script the system is legitimately
    \* quiescent; allow a stutter step there so TLC's deadlock detector only
    \* fires on genuinely stuck states (e.g. a drain that can never complete).
    \/ (Done /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

-----------------------------------------------------------------------------
(* Properties.                                                               *)

(* 1. The slot's value always conforms to the currently published            *)
(*    restriction.                                                           *)
TypeSlotOK == Conforms(slot, eps[pub].ty)

(* 2. THE per-partition theorem: an epoch whose read flag is clear can       *)
(*    trust the slot -- for *every* epoch, at *every* moment (the flag        *)
(*    calculus and the fence/drain ordering conspire to keep this true even  *)
(*    while a re-declaration is mid-flight).                                 *)
ClearReadFlagsSound ==
    \A i \in 1..Len(eps) : ~eps[i].r => Conforms(slot, eps[i].ty)

(* 3. Its write-side counterpart: an epoch whose write flag is clear has a   *)
(*    restriction the published one still accepts, so values it validated    *)
(*    remain storable.                                                       *)
ClearWriteFlagsSound ==
    \A i \in 1..Len(eps) : ~eps[i].w => Subtype(eps[i].ty, eps[pub].ty)

(* 4. Whenever the reader reaches r3 having observed a CLEAR flag, the value *)
(*    it read at r1 conforms to the declared type it was compiled against.   *)
ReaderSoundness == (rd.pc = "r3" /\ ~rd.flg) => Conforms(rd.val, eps[rd.cep].ty)

(* 5. No fast-path commit ever stored a value that did not conform, at       *)
(*    commit time, to the then-published restriction.                        *)
StoreValidation == ~badStore

(* Liveness sanity check (with WF): every fair behavior finishes all         *)
(* scripts, in particular every drain terminates.                           *)
Termination == <>[]Done

=============================================================================
