# SROA on UnifiedIR (§10.4; stock reference: Compiler/src/ssair/passes.jl
# `sroa_pass!`/`sroa_mutables!` — the CASES, not the mechanics):
#
#   1. Immutable-struct SROA: `extract` (canonicalized getfield) of a locally
#      constructed `Core.tuple`/`K"new"` of a concrete immutable type forwards
#      the field value, following `refine` chains; legality of the forwarded
#      operand at the use site is checked with `UnifiedIR.visible`.
#      (Extension of `forward_extracts!`, which lives in optimize.jl.)
#   2. If-result forwarding: an `if` whose live arms all produce the same
#      operand forwards that operand to the result's uses (the phi-of-one-
#      value case of stock SROA lifting).
#   3. Mutable-struct SROA: a `new` of a mutable struct that never escapes
#      (uses are only getfield/extract loads and setfield! stores with
#      constant fields) becomes per-field cells (K"cell" + cell_set/cell_get);
#      `UnifiedIR.promote_cells!` + `dce!` then clean up.
#   4. Dead `new` elimination falls out of `dce!` once `refine_effects!`
#      marks nothrow constructions REMOVABLE (optimize.jl).

"Field index (1-based) for a constant field designator (Int or Symbol), or nothing."
function field_index_of(@nospecialize(T), @nospecialize(fld))
    T isa DataType || return nothing
    if fld isa Int
        1 <= fld <= fieldcount(T) || return nothing
        return fld
    elseif fld isa Symbol
        fi = Base.fieldindex(T, fld, false)
        return fi == 0 ? nothing : fi
    end
    return nothing
end

"Concrete DataType of a lattice element/operand type, or nothing."
function concrete_datatype(@nospecialize(tl))
    T = tl isa CC.Const ? tl.val : CC.singleton_type(CC.widenconst(tl))
    if T === nothing
        wt = CC.widenconst(tl)
        wt isa DataType && isconcretetype(wt) && (T = wt)
    end
    T isa DataType && isconcretetype(T) || return nothing
    return T
end

"Skip through K\"refine\" chains to the underlying definition."
function skip_refines(ir::UnifiedIR.IR, def::StmtId)
    steps = 0
    while UnifiedIR.stmt_kind(ir, def) === K"refine" && (steps += 1) <= 32
        o = UnifiedIR.getop(ir, def, 1)
        UnifiedIR.optag(o) == UnifiedIR.TAG_STMT || break
        def = UnifiedIR.asstmt(o)
    end
    return def
end
"""
    sroa_mutables!(ir) -> Int

Mutable-struct SROA (§10.4 / stock `sroa_mutables!` cases): fully-initialized
`new` of a concrete mutable struct whose value never escapes — every use is
`extract`/`getfield(it, const fld)` or `setfield!(it, const fld, v)` — is
replaced by per-field frame cells. Loads become `cell_get`, stores become
`cell_set` (+ a `refine` carrying setfield!'s value result). Editable state;
`promote_cells!`/`dce!` finish the job on the next dense round.
"""
function sroa_mutables!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "sroa_mutables!")
    promoted = 0
    for s in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, s) && continue
        UnifiedIR.stmt_kind(ir, s) === K"new" || continue
        T = concrete_datatype(stmt_lattice(ir, UnifiedIR.getop(ir, s, 1)))
        (T isa DataType && ismutabletype(T)) || continue
        nf = fieldcount(T)
        UnifiedIR.nops(ir, s) - 1 == nf || continue   # fully-initialized only (v1)
        any(i -> Base.isfieldatomic(T, i), 1:nf) && continue
        # inside a cfg island the replacement cells could never promote
        # (promote_cells! §6 policy refuses island cells) — a pure
        # pessimization; leave the allocation in memory form there
        UnifiedIR.inside_island(ir, s) && continue
        # collect uses; any non-load/store use disqualifies (escape check)
        loads = Tuple{StmtId,Int}[]
        stores = Tuple{StmtId,Int}[]
        ok = true
        UnifiedIR.each_ssa_use(ir) do site, used
            (ok && used == s) || return
            site isa UnifiedIR.StmtOperand || (ok = false; return)
            u = site.user
            UnifiedIR.is_tombstone(ir, u) && return
            uk = UnifiedIR.stmt_kind(ir, u)
            if uk === K"extract" && site.opidx == 1
                idx = Int(UnifiedIR.imm_value(UnifiedIR.getop(ir, u, 2))::Int64)
                1 <= idx <= nf ? push!(loads, (u, idx)) : (ok = false)
            elseif uk === K"call"
                callee = static_operand_value(ir, UnifiedIR.getop(ir, u, 1))
                nopu = UnifiedIR.nops(ir, u)
                if (callee === Core.getfield || callee === Base.getfield) &&
                   site.opidx == 2 && (nopu == 3 || nopu == 4)
                    fld = field_index_of(T, static_operand_value(ir, UnifiedIR.getop(ir, u, 3)))
                    fld === nothing ? (ok = false) : push!(loads, (u, fld))
                elseif (callee === Core.setfield! || callee === Base.setfield!) &&
                       site.opidx == 2 && nopu == 4
                    fld = field_index_of(T, static_operand_value(ir, UnifiedIR.getop(ir, u, 3)))
                    fld === nothing ? (ok = false) : push!(stores, (u, fld))
                else
                    ok = false
                end
            else
                ok = false   # cell_set, return, result, phi-ish, nested call arg, …
            end
        end
        ok || continue
        # rewrite: per-field cells + initial stores, placed just before the new
        cells = StmtId[]
        for i in 1:nf
            ft = fieldtype(T, i)
            c = UnifiedIR.insert_before!(ir, s, K"cell", UnifiedIR.vop(ir, ft); type = ft)
            push!(cells, c)
            UnifiedIR.insert_before!(ir, s, K"cell_set", UnifiedIR.op_stmt(c),
                                     UnifiedIR.getop(ir, s, i + 1))
        end
        for (u, fld) in loads
            UnifiedIR.replace_stmt!(ir, u, K"cell_get", UnifiedIR.op_stmt(cells[fld]);
                                    type = UnifiedIR.stmt_type(ir, u))
        end
        for (u, fld) in stores
            vo = UnifiedIR.getop(ir, u, 4)
            UnifiedIR.insert_before!(ir, u, K"cell_set", UnifiedIR.op_stmt(cells[fld]), vo)
            # setfield! evaluates to the stored value; keep that result shape
            UnifiedIR.replace_stmt!(ir, u, K"refine", vo; type = UnifiedIR.stmt_type(ir, u))
        end
        UnifiedIR.kill_stmt!(ir, s)
        promoted += 1
    end
    return promoted
end

# ---------------------------------------------------------------------------
# The cell-promotion mem2reg suite lives in the substrate now
# (UnifiedIR/src/promote.jl) so lowering's capture analysis runs the SAME
# machinery. Bind the names this module's passes, tests, and harnesses use;
# `promote_loop_cells!` gets the inferred-Const reader as its static-value
# hook (the substrate default cannot see the type lattice).
# ---------------------------------------------------------------------------

const forward_if_results!   = UnifiedIR.forward_if_results!
const promote_block_cells!  = UnifiedIR.promote_block_cells!
const promote_arm_cells!    = UnifiedIR.promote_arm_cells!
const promote_island_cells! = UnifiedIR.promote_island_cells!
const promote_undef_cells!  = UnifiedIR.promote_undef_cells!
const PROMOTION_TRACE       = UnifiedIR.PROMOTION_TRACE
const _in_handler           = UnifiedIR._in_handler
const is_diverge_kind       = UnifiedIR.is_diverge_kind

"Inferred-Const static value of a statement (the lattice-aware `stmt_value` hook)."
function _stmt_const_value(ir::UnifiedIR.IR, s::StmtId)
    tt = UnifiedIR.stmt_type(ir, s)
    tt isa CC.Const && return tt.val
    return CC.singleton_type(tt isa Type ? tt : Any)
end

promote_loop_cells!(ir::UnifiedIR.IR) =
    UnifiedIR.promote_loop_cells!(ir; stmt_value = _stmt_const_value)

# `promote_fixpoint!` is exported by UnifiedIR (visible here via `using`);
# call it with `stmt_value = _stmt_const_value` to give it the lattice.
