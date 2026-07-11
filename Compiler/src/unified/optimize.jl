# The optimizer port (§10.4): passes over typed UnifiedIR. Simple passes on
# dense state; branch folding and inlining through editable sessions and
# splice_body! — the two-phase mutate-then-compact discipline, reformed.

"""
    refine_effects!(ir) -> Int

Set per-statement effect flags from inferred information: builtin calls get
`Compiler.builtin_effects`-derived flags; Const-typed statements of builtin
provenance become foldable; nothrow `new` constructions of concrete types
become REMOVABLE (dead-`new` elimination then falls out of `dce!` — SROA
deliverable 1c). Returns statements refined.
"""
function refine_effects!(ir::UnifiedIR.IR; interp = CC.NativeInterpreter())
    n = 0
    for s in UnifiedIR.each_stmt(ir)
        k = UnifiedIR.stmt_kind(ir, s)
        if k === K"new"
            T = concrete_datatype(stmt_lattice(ir, UnifiedIR.getop(ir, s, 1)))
            T isa DataType || continue
            nsupplied = UnifiedIR.nops(ir, s) - 1
            nsupplied <= fieldcount(T) || continue    # over-arity `new` throws
            nothrow = true
            for i in 1:nsupplied
                at = CC.widenconst(stmt_lattice(ir, UnifiedIR.getop(ir, s, i + 1)))
                (at isa Type && at <: fieldtype(T, i)) || (nothrow = false; break)
            end
            nothrow || continue
            flags = UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_NOTHROW |
                    UnifiedIR.FLAG_TERMINATES
            ismutabletype(T) || (flags |= UnifiedIR.FLAG_CONSISTENT)
            if flags != UnifiedIR.stmt_flag(ir, s)
                UnifiedIR.set_flag!(ir, s, flags)
                n += 1
            end
            continue
        end
        if k === K"globalref"
            # reads of constant, defined bindings are foldable
            o = UnifiedIR.getop(ir, s, 1)
            UnifiedIR.optag(o) == UnifiedIR.TAG_GLOBAL || continue
            g = ir.body.globals[UnifiedIR.payload(o)]
            (isconst(g.mod, g.name) && isdefined(g.mod, g.name)) || continue
            flags = UnifiedIR.FLAG_CONSISTENT | UnifiedIR.FLAG_REMOVABLE
            if flags != UnifiedIR.stmt_flag(ir, s)
                UnifiedIR.set_flag!(ir, s, flags)
                n += 1
            end
            continue
        end
        (k === K"call" || k === K"intrinsic") || continue
        fo = UnifiedIR.getop(ir, s, 1)
        fl = static_operand_value(ir, fo)
        fl isa Core.Builtin || continue
        argl = Any[stmt_lattice(ir, UnifiedIR.getop(ir, s, i)) for i in 2:UnifiedIR.nops(ir, s)]
        rt = UnifiedIR.stmt_type(ir, s)
        effects = try
            CC.builtin_effects(CC.fallback_lattice, fl, argl, rt isa Type ? rt : Any)
        catch
            continue
        end
        flags = UInt32(0)
        CC.is_consistent(effects) && (flags |= UnifiedIR.FLAG_CONSISTENT)
        CC.is_effect_free(effects) && (flags |= UnifiedIR.FLAG_EFFECT_FREE)
        CC.is_nothrow(effects) && (flags |= UnifiedIR.FLAG_NOTHROW)
        CC.is_terminates(effects) && (flags |= UnifiedIR.FLAG_TERMINATES)
        if flags != UnifiedIR.stmt_flag(ir, s)
            UnifiedIR.set_flag!(ir, s, flags)
            n += 1
        end
    end
    return n
end

"Constant value of an operand, or nothing (statements consult the type column)."
function static_operand_value(ir::UnifiedIR.IR, o::UnifiedIR.Operand)
    t = UnifiedIR.optag(o)
    if t == UnifiedIR.TAG_INLINE
        return UnifiedIR.imm_value(o)
    elseif t == UnifiedIR.TAG_CONST
        return ir.body.constants[UnifiedIR.payload(o)]
    elseif t == UnifiedIR.TAG_GLOBAL
        g = ir.body.globals[UnifiedIR.payload(o)]
        (isconst(g.mod, g.name) && isdefined(g.mod, g.name)) && return getglobal(g.mod, g.name)
        return nothing
    elseif t == UnifiedIR.TAG_STMT
        tt = UnifiedIR.stmt_type(ir, UnifiedIR.asstmt(o))
        tt isa CC.Const && return tt.val
        st = CC.singleton_type(tt isa Type ? tt : Any)
        return st
    end
    return nothing
end

function stmt_lattice(ir::UnifiedIR.IR, o::UnifiedIR.Operand)
    t = UnifiedIR.optag(o)
    if t == UnifiedIR.TAG_STMT
        tt = UnifiedIR.stmt_type(ir, UnifiedIR.asstmt(o))
        return tt === nothing ? Any : tt
    elseif t == UnifiedIR.TAG_INLINE
        return CC.Const(UnifiedIR.imm_value(o))
    elseif t == UnifiedIR.TAG_CONST
        return CC.Const(ir.body.constants[UnifiedIR.payload(o)])
    elseif t == UnifiedIR.TAG_GLOBAL
        g = ir.body.globals[UnifiedIR.payload(o)]
        (isconst(g.mod, g.name) && isdefined(g.mod, g.name)) &&
            return CC.Const(getglobal(g.mod, g.name))
        return Any
    end
    return Any
end

"""
    materialize_consts!(ir) -> Int

Replace statements whose inferred type is `Const(v)` and whose flags satisfy
the foldable mask with `K"value"` constants (footprint-preserving), and
forward the constant directly into the use sites so DCE can delete the whole
chain. Skips identity-bearing constants.
"""
function materialize_consts!(ir::UnifiedIR.IR)
    n = 0
    foldable = UnifiedIR.FLAG_CONSISTENT | UnifiedIR.FLAG_EFFECT_FREE |
               UnifiedIR.FLAG_NOTHROW | UnifiedIR.FLAG_TERMINATES
    for s in UnifiedIR.each_stmt(ir)
        k = UnifiedIR.stmt_kind(ir, s)
        UnifiedIR.result_arity(k) == 1 || continue
        UnifiedIR.owns_regions(k) && continue
        (k === K"value" || k === K"region_arg" || k === K"cell" || k === K"cell_shared") && continue
        t = UnifiedIR.stmt_type(ir, s)
        t isa CC.Const || continue
        v = t.val
        ismutable(v) && !(v isa Union{Type,Function,Module,Symbol,String}) && continue
        UnifiedIR.stmt_flag(ir, s) & foldable == foldable || continue
        # K"value" requires a pool constant (its schema is OC_CONST)
        co = UnifiedIR.op_constidx(UnifiedIR.intern_const!(ir.body, v))
        UnifiedIR.replace_stmt!(ir, s, K"value", co; type = t)
        UnifiedIR.replace_uses!(ir, s => UnifiedIR.vop(ir, v))
        n += 1
    end
    n > 0 && UnifiedIR.flush_renames!(ir)
    return n
end

"""
    forward_refines!(ir) -> Int

Uses of a `refine` that adds no type information over its operand are
rewritten to the operand (the #54762 Pi-accumulation cleanup, per the
`refine` canonicalizability note in §5.8); constant-operand refines forward
unconditionally. Genuinely narrowing refines (union-split arms) are kept.
"""
function forward_refines!(ir::UnifiedIR.IR)
    n = 0
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.stmt_kind(ir, s) === K"refine" || continue
        o = UnifiedIR.getop(ir, s, 1)
        t = UnifiedIR.optag(o)
        if t == UnifiedIR.TAG_STMT
            d = UnifiedIR.asstmt(o)
            rt = CC.widenconst(UnifiedIR.stmt_type(ir, s))
            dt = CC.widenconst(UnifiedIR.stmt_type(ir, d))
            (rt isa Type && dt isa Type && dt <: rt) || continue   # narrows: keep
            UnifiedIR.replace_uses!(ir, s => o)
            n += 1
        elseif t == UnifiedIR.TAG_CONST || t == UnifiedIR.TAG_INLINE
            UnifiedIR.replace_uses!(ir, s => o)
            n += 1
        end
    end
    n > 0 && UnifiedIR.flush_renames!(ir)
    return n
end

"""
    canonicalize_getfields!(ir) -> Int

Julia-dialect canonicalization (§3.2): `getfield(x, fld::Const)` calls —
integer index, or symbol on a value of known concrete type, optionally with a
trailing boundscheck operand — become the explicit `K"extract"` kind
(inline-encoded, index 0-based).
"""
function canonicalize_getfields!(ir::UnifiedIR.IR)
    n = 0
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.stmt_kind(ir, s) === K"call" || continue
        nop = UnifiedIR.nops(ir, s)
        (nop == 3 || nop == 4) || continue
        callee = static_operand_value(ir, UnifiedIR.getop(ir, s, 1))
        callee === Core.getfield || callee === Base.getfield || continue
        vo = UnifiedIR.getop(ir, s, 2)
        UnifiedIR.optag(vo) == UnifiedIR.TAG_STMT || continue
        io = UnifiedIR.getop(ir, s, 3)
        idx = static_operand_value(ir, io)
        if idx isa Symbol
            xt = CC.widenconst(stmt_lattice(ir, vo))
            xt isa DataType && isconcretetype(xt) || continue
            idx = field_index_of(xt, idx)
            idx === nothing && continue
        end
        idx isa Int || continue
        idx >= 1 || continue
        idx - 1 < (1 << 23) || continue
        UnifiedIR.replace_stmt!(ir, s, K"extract", vo, UnifiedIR.op_inline(idx - 1);
                                type = UnifiedIR.stmt_type(ir, s))
        n += 1
    end
    return n
end

"""
    forward_extracts!(ir) -> Int

Immutable-struct SROA, load-forwarding case (deliverable 1a): `extract(x, i)`
of a locally-constructed `call Core.tuple(a...)` or `K"new"` of a concrete
*immutable* type — following `refine` chains — becomes `refine a[i]`.
Legality of the forwarded operand at the use site is checked with
`UnifiedIR.visible` (§5.1).
"""
function forward_extracts!(ir::UnifiedIR.IR)
    n = 0
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.stmt_kind(ir, s) === K"extract" || continue
        vo = UnifiedIR.getop(ir, s, 1)
        UnifiedIR.optag(vo) == UnifiedIR.TAG_STMT || continue
        def = skip_refines(ir, UnifiedIR.asstmt(vo))
        idx = Int(UnifiedIR.imm_value(UnifiedIR.getop(ir, s, 2))::Int64)
        dk = UnifiedIR.stmt_kind(ir, def)
        local el::UnifiedIR.Operand
        if dk === K"call"
            callee = static_operand_value(ir, UnifiedIR.getop(ir, def, 1))
            callee === Core.tuple || continue
            1 + idx + 1 <= UnifiedIR.nops(ir, def) || continue
            el = UnifiedIR.getop(ir, def, idx + 2)
        elseif dk === K"new"
            T = concrete_datatype(stmt_lattice(ir, UnifiedIR.getop(ir, def, 1)))
            (T isa DataType && !ismutabletype(T)) || continue
            idx + 1 <= UnifiedIR.nops(ir, def) - 1 || continue  # field must be supplied
            el = UnifiedIR.getop(ir, def, idx + 2)
        else
            continue
        end
        # the forwarded operand must be visible at the extract (§5.1 all three
        # clauses); constants/globals/immediates are always legal
        if UnifiedIR.optag(el) == UnifiedIR.TAG_STMT
            UnifiedIR.visible(ir, UnifiedIR.asstmt(el), s) || continue
        end
        UnifiedIR.replace_stmt!(ir, s, K"refine", el; type = UnifiedIR.stmt_type(ir, s))
        n += 1
    end
    return n
end

"""
    optimize_ir!(ir, argtypes; state, inline=true, rounds=8, params) -> ir

The pipeline (§10.4), iterated to quiescence. Per round:

  dense:    inference → effects refinement (incl. `new` removability) →
            const materialization → getfield canonicalization → extract
            forwarding (immutable SROA) → refine forwarding → if-result
            forwarding → cell promotion (region-tree + single-region) → DCE
  editable: constant-branch folding → island branch folding → unreachable-
            block pruning → goto-chain merging → structurization (§10.5:
            if/loop recovery from islands) → island dissolution → loop-
            carried cell promotion → select conversion → mutable-struct
            SROA → region-op ADCE → inlining (calls + invokes) → union
            splitting
  compact! + verify (level 1)
"""
function optimize_ir!(ir::UnifiedIR.IR, argtypes::Vector{Any};
                      state::UInferState = UInferState(), inline::Bool = true,
                      rounds::Int = 8, params::InlineParams = InlineParams())
    for round in 1:rounds
        changed = 0
        infer_ir!(ir, argtypes; state)
        changed += refine_effects!(ir)
        changed += materialize_consts!(ir)
        changed += canonicalize_getfields!(ir)
        changed += forward_extracts!(ir)
        changed += forward_refines!(ir)
        changed += forward_if_results!(ir)
        changed += UnifiedIR.promote_cells!(ir)
        changed += promote_block_cells!(ir)
        changed += UnifiedIR.dce!(ir)
        UnifiedIR.editable(ir)
        _, folded = UnifiedIR.fold_constant_branches!(ir)
        changed += folded
        changed += fold_island_branches!(ir)
        changed += drop_unreachable_blocks!(ir)
        changed += merge_goto_chains!(ir)
        changed += structurize!(ir)
        changed += dissolve_islands!(ir)
        # joint cell-promotion fixpoint (§6 join completeness, docs
        # "Join-point completeness"): arm-join sinking turns conditional arm
        # stores into unconditional post-join stores, which loop promotion
        # consumes as carried values and (next round) promote_cells! as
        # dominating stores — and each can expose new cases for the others.
        while true
            c = promote_arm_cells!(ir)
            c += promote_island_cells!(ir)
            c += promote_loop_cells!(ir)
            c == 0 && break
            changed += c
        end
        changed += selectify!(ir)
        changed += sroa_mutables!(ir)
        changed += adce_region_ops!(ir)
        if inline
            changed += inline_calls2!(ir, state; params)
            changed += union_split_calls!(ir, state; params)
        end
        ir, _ = UnifiedIR.compact!(ir)
        UnifiedIR.verify_ir(ir; level = 1)
        changed == 0 && break
    end
    infer_ir!(ir, argtypes; state)
    UnifiedIR.verify_ir(ir; level = 1)
    return ir
end
