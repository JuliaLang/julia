# Verification layers (§11.2). L0: structural, O(n), always cheap.
# L1: SSA/region semantics (visibility, terminators, arities, activations).

struct VerifyError <: Exception
    msg::String
end
Base.showerror(io::IO, e::VerifyError) = print(io, "VerifyError: ", e.msg)

verr(msg...) = throw(VerifyError(string(msg...)))

"""
    verify_ir(ir; level=1)

Run verification. `level=0`: structural only. `level=1`: adds the SSA/region
semantic rules. Throws `VerifyError` on the first violation.
"""
function verify_ir(ir::IR; level::Int = 1)
    verify_l0(ir)
    level >= 1 && verify_l1(ir)
    return true
end

function verify_l0(ir::IR)
    body = ir.body
    n = Int(body.len)
    # column lengths agree
    for (name, v) in ((:kind, body.kind), (:ops, body.ops), (:type, body.type),
                      (:flag, body.flag), (:debug, body.debug), (:region, body.region))
        length(v) == n || verr("column $name length $(length(v)) != len $n")
    end
    # kinds registered; operand words well-formed; result-arity discipline
    for i in 1:n
        k = body.kind[i]
        info = try
            kindinfo(k)
        catch
            verr("stmt %$i: unregistered kind $k")
        end
        k === KIND_DELETED && continue
        s = StmtId(i)
        w = body.ops[i]
        if is_ops_inline(w)
            has_inline_ops(k) || verr("stmt %$i ($(info.qualified)): inline ops word on non-inline kind")
        else
            off, len = ops_offset(w), ops_len(w)
            off + len <= length(body.operands) || verr("stmt %$i: operand range out of pool bounds")
        end
        nop = nops(ir, s)
        (nop >= info.minops && (info.maxops < 0 || nop <= info.maxops)) ||
            verr("stmt %$i ($(info.qualified)): arity $nop outside [$(info.minops), $(info.maxops)]")
        for j in 1:nop
            o = getop(ir, s, j)
            t = optag(o)
            if t == TAG_STMT
                1 <= payload(o) <= n || verr("stmt %$i operand $j: stmt reference out of range")
                target = StmtId(payload(o) % Int32)
                body.kind[target.id] === KIND_DELETED &&
                    verr("stmt %$i operand $j: use of tombstoned %$(target.id)")
                result_arity(body.kind[target.id]) == 0 &&
                    verr("stmt %$i operand $j: reference to zero-result stmt %$(target.id)")
            elseif t == TAG_CONST
                1 <= payload(o) <= length(body.constants) || verr("stmt %$i: constant index out of range")
            elseif t == TAG_GLOBAL
                1 <= payload(o) <= length(body.globals) || verr("stmt %$i: global index out of range")
            elseif t == TAG_REGION || t == TAG_BLOCK
                1 <= payload(o) <= length(ir.regions) || verr("stmt %$i: region reference out of range")
            end
        end
        # escape-hatch constants must be reference-free leaves (§3.2)
        if k === K"value"
            v = getconst(ir, getop(ir, s, 1))
            refbearing_value(v) && verr("stmt %$i: K\"value\" payload embeds IR references without a codec")
        end
        # regions column in range
        1 <= body.region[i].id <= length(ir.regions) || verr("stmt %$i: region out of range")
    end
    # region table
    nr = length(ir.regions)
    nr >= 1 || verr("no root region")
    for (ri, reg) in enumerate(ir.regions)
        # parent links acyclic and in-range
        if ri == 1
            isnull(reg.parent) || verr("root region has a parent")
            isnull(reg.owner) || verr("root region has an owner")
        else
            (1 <= reg.parent.id <= nr) || verr("region ^r$ri: parent out of range")
            # acyclicity: walk up with a step bound
            steps = 0
            p = reg.parent
            while !isnull(p)
                steps += 1
                steps > nr && verr("region parent chain cycle at ^r$ri")
                p = getregion(ir, p).parent
            end
        end
        if is_guard(reg)
            isnull(reg.owner) || verr("guard region ^r$ri has an owner")
        elseif ri != 1
            isnull(reg.owner) && verr("ordered region ^r$ri has no owner")
            owns_regions(stmt_kind(ir, reg.owner)) ||
                verr("region ^r$ri: owner kind $(kindname(stmt_kind(ir, reg.owner))) does not own regions")
            stmt_region(ir, reg.owner) == reg.parent ||
                verr("region ^r$ri: owner's region is not the parent")
        end
        if !is_guard(reg) && !isnull(reg.cond)
            verr("owned region ^r$ri stores a condition (guard regions only)")
        end
        # region_args lead the region and match args list
        for a in reg.args
            (1 <= a.id <= n) || verr("region ^r$ri: arg out of range")
            body.kind[a.id] === K"region_arg" || verr("region ^r$ri: arg %$(a.id) is not a region_arg")
            body.region[a.id] == RegionId(ri) || verr("region ^r$ri: arg %$(a.id) not in region")
        end
    end
    if layout(ir) === LAYOUT_DENSE || layout(ir) === LAYOUT_BUILDER
        verify_l0_dense(ir)
    elseif layout(ir) === LAYOUT_EDITABLE
        verify_l0_editable(ir)
    end
    return true
end

# Is this value allowed as a K"value" escape payload? Reference-free leaves only.
refbearing_value(@nospecialize(v)) =
    v isa StmtId || v isa RegionId || v isa Operand ||
    (v isa Union{Tuple,AbstractArray} && any(refbearing_value, v))

function verify_l0_dense(ir::IR)
    body = ir.body
    n = Int(body.len)
    # spans contiguous, properly nested; region_args leading; owned regions
    # immediately follow their owner; one terminator ends each ordered region.
    for (ri, reg) in enumerate(ir.regions)
        is_guard(reg) && continue
        lo, hi = reg.first.id, reg.last.id
        if hi < lo
            # empty region: illegal for ordered regions in sealed dense state
            layout(ir) === LAYOUT_DENSE && verr("region ^r$ri is empty")
            continue
        end
        (1 <= lo <= hi <= n) || verr("region ^r$ri: span [$lo, $hi] out of range")
        # every stmt in span belongs to this region or a (transitive) descendant
        for i in lo:hi
            r = body.region[i]
            is_ancestor(ir, RegionId(ri), r) || verr("stmt %$i inside span of ^r$ri but not a descendant")
        end
        # region_args lead
        args_done = false
        for i in lo:hi
            body.region[i] == RegionId(ri) || continue
            if body.kind[i] === K"region_arg"
                args_done && verr("region ^r$ri: region_arg %$i after non-arg stmts")
            else
                args_done = true
            end
        end
        # terminator discipline (sealed dense only; builder may be mid-build)
        if layout(ir) === LAYOUT_DENSE
            # last direct member must be the unique terminator
            lastdirect = 0
            nterm = 0
            for i in lo:hi
                body.region[i] == RegionId(ri) || continue
                body.kind[i] === KIND_DELETED && continue
                lastdirect = i
                is_terminator(body.kind[i]) && (nterm += 1)
            end
            if RegionId(ri) != root_region(ir) || rootbody_needs_terminator(ir)
                lastdirect == 0 && verr("region ^r$ri has no statements")
                is_terminator(body.kind[lastdirect]) ||
                    verr("region ^r$ri does not end in a terminator")
                nterm == 1 || verr("region ^r$ri has $nterm terminators (want exactly 1)")
            end
        end
    end
    # owned regions contiguous immediately after their owner, in table order
    if layout(ir) === LAYOUT_DENSE
        for i in 1:n
            body.kind[i] === KIND_DELETED && continue
            owns_regions(body.kind[i]) || continue
            s = StmtId(i)
            pos = i + 1
            for rid in owned_regions(ir, s)
                reg = getregion(ir, rid)
                reg.first.id == pos ||
                    verr("stmt %$i: owned region ^r$(rid.id) starts at $(reg.first.id), expected $pos")
                pos = reg.last.id + 1
            end
        end
    end
    return true
end

# Floating-node bodies and builder intermediates have no terminator requirement
# on the root; a sealed dense function body must end in a terminator.
rootbody_needs_terminator(ir::IR) = get(ir.meta, :floating_node, false) === false

function verify_l0_editable(ir::IR)
    e = ir.edit
    e === nothing && verr("editable state without EditState")
    body = ir.body
    n = Int(body.len)
    (length(e.next) == n && length(e.prev) == n && length(e.okey) == n) ||
        verr("edit lists length mismatch")
    # per-region list consistency + okey increasing along links
    for (ri, reg) in enumerate(ir.regions)
        is_guard(reg) && continue
        i = reg.first.id
        prev = 0
        seen = 0
        while i != 0
            seen += 1
            seen > n && verr("region ^r$ri: list cycle")
            body.region[i] == RegionId(ri) || verr("region ^r$ri list contains foreign stmt %$i")
            e.prev[i] == prev || verr("region ^r$ri: prev link broken at %$i")
            if prev != 0
                e.okey[prev] < e.okey[i] || verr("region ^r$ri: okey not increasing at %$i")
            end
            prev = i
            i = e.next[i]
        end
        reg.last.id == prev || (reg.first.id == 0 && reg.last.id == 0) ||
            verr("region ^r$ri: tail mismatch")
    end
    return true
end

# ---------------------------------------------------------------------------
# L1: SSA/region semantic rules (§11.2)
# ---------------------------------------------------------------------------

function verify_l1(ir::IR)
    body = ir.body
    n = Int(body.len)
    floating = layout(ir) === LAYOUT_FLOATING

    # visibility for every ssa_use-role reference (all three clauses)
    visit_refs(ir) do role, site, o
        role === REF_SSA_USE || return
        optag(o) == TAG_STMT || return
        def = asstmt(o)
        if site isa StmtOperand
            visible(ir, def, site.user) ||
                verr("stmt %$(site.user.id) operand $(site.opidx): $(def) is not visible (§5.1)")
        elseif site isa GuardCondition
            # guard condition must be visible at the region's parent scope
            reg = getregion(ir, site.region)
            dr = stmt_region(ir, def)
            is_ancestor(ir, dr, reg.parent) ||
                verr("guard ^r$(site.region.id): condition $(def) not defined in an ancestor region")
        end
    end

    # exit-terminator legality; region-arg arities; activation boundaries
    for i in 1:n
        k = body.kind[i]
        k === KIND_DELETED && continue
        s = StmtId(i)
        if k === K"continue" || k === K"break"
            tgt = asregion(getop(ir, s, 1))
            treg = getregion(ir, tgt)
            treg.kind === REGION_LOOP_BODY ||
                verr("%$i: $(kindname(k)) target ^r$(tgt.id) is not a loop body")
            is_ancestor(ir, tgt, stmt_region(ir, s)) ||
                verr("%$i: $(kindname(k)) targets non-ancestor region ^r$(tgt.id)")
            # activation boundary: target must be within the same activation
            activation_root(ir, stmt_region(ir, s)) == activation_root(ir, tgt) ||
                verr("%$i: $(kindname(k)) crosses an activation boundary")
            if k === K"continue"
                nvals = nops(ir, s) - 2
                nvals == length(treg.args) ||
                    verr("%$i: continue carries $nvals values for $(length(treg.args)) carried args")
            end
        elseif k === K"return"
            ar = activation_root(ir, stmt_region(ir, s))
            ar == root_region(ir) || getregion(ir, ar).activation !== ACT_IMMEDIATE ||
                verr("%$i: return outside function/closure body")
        elseif k === K"yield" && !floating
            reg = getregion(ir, stmt_region(ir, s))
            isnull(reg.owner) && stmt_region(ir, s) != root_region(ir) &&
                verr("%$i: yield in ownerless region")
        elseif k === K"goto" || k === K"br_if" || k === K"switch" || k === K"await"
            # every BLOCK operand must target a block region of the same (or
            # ancestor, for goto) cfg island; edge arity checked below
            verify_edges(ir, s)
        end
    end

    # cfg edge bundles match destination block args (§5.5)
    for i in 1:n
        k = body.kind[i]
        (k === K"goto" || k === K"br_if" || k === K"switch" || k === K"await") || continue
        s = StmtId(i)
        for (dest, args) in edge_bundles(ir, s)
            dreg = getregion(ir, dest)
            dreg.kind === REGION_BLOCK || verr("%$i: edge target ^r$(dest.id) is not a block")
            length(args) == length(dreg.args) ||
                verr("%$i: edge to ^r$(dest.id) carries $(length(args)) args for $(length(dreg.args)) block args")
        end
    end

    # gc_preserve pairing within one region — or within one cfg island, where
    # cross-block pairing shares the island's dynamic scope
    for i in 1:n
        body.kind[i] === K"gc_preserve_end" || continue
        s = StmtId(i)
        tok = asstmt(getop(ir, s, 1))
        # tokens may be slot/cell-carried in converted input: a cell_get whose
        # every store writes a begin is a valid (indirected) pairing link
        if stmt_kind(ir, tok) === K"cell_get"
            cellid = asstmt(getop(ir, tok, 1))
            allbegin = true
            anystore = false
            for j in 1:n
                body.kind[j] === K"cell_set" || continue
                asstmt(getop(ir, StmtId(j), 1)) == cellid || continue
                anystore = true
                v = getop(ir, StmtId(j), 2)
                (optag(v) == TAG_STMT &&
                 stmt_kind(ir, asstmt(v)) === K"gc_preserve_begin") || (allbegin = false)
            end
            (anystore && allbegin) ||
                verr("%$i: gc_preserve_end cell token is not begin-only")
            continue
        end
        stmt_kind(ir, tok) === K"gc_preserve_begin" ||
            verr("%$i: gc_preserve_end token is not a gc_preserve_begin")
        if stmt_region(ir, tok) != stmt_region(ir, s)
            # legal when the begin is visible at the end (ancestor region, or
            # dominating island block); EH scope recovery can also place the
            # pair in sibling scopes of one try — accept a shared try ancestor
            if !visible(ir, tok, s)
                sharedtry = false
                r1 = stmt_region(ir, tok)
                while !isnull(r1)
                    reg1 = getregion(ir, r1)
                    if !isnull(reg1.owner) && stmt_kind(ir, reg1.owner) === K"try"
                        if is_ancestor(ir, stmt_region(ir, reg1.owner), stmt_region(ir, s))
                            sharedtry = true
                            break
                        end
                    end
                    r1 = reg1.parent
                end
                sharedtry || verr("%$i: gc_preserve pair split across regions")
            end
        end
    end

    floating && verify_floating_acyclic(ir)
    return true
end

function verify_edges(ir::IR, s::StmtId)
    for (dest, _) in edge_bundles(ir, s)
        1 <= dest.id <= length(ir.regions) || verr("%$(s.id): edge target out of range")
    end
end

"""
    edge_bundles(ir, s) -> Vector{Tuple{RegionId,Vector{Operand}}}

Decode the successor edge bundles of a cfg terminator (§3.2 physical
encoding): `goto (BLOCK, argc, args…)`; `br_if (cond, then-bundle,
else-bundle)`; `switch (val, ncases, {caseval, bundle}…, default-bundle)`;
`await (flags, normal-bundle, resume-bundle)`.
"""
function edge_bundles(ir::IR, s::StmtId)
    k = stmt_kind(ir, s)
    out = Tuple{RegionId,Vector{Operand}}[]
    i = 1
    n = nops(ir, s)
    readbundle() = begin
        dest = asregion(getop(ir, s, i)); i += 1
        argc = Int(imm_value(getop(ir, s, i))::Int64); i += 1
        args = Operand[getop(ir, s, i + j - 1) for j in 1:argc]
        i += argc
        (dest, args)
    end
    if k === K"goto"
        push!(out, readbundle())
    elseif k === K"br_if"
        i = 2  # skip cond
        push!(out, readbundle())
        push!(out, readbundle())
    elseif k === K"switch"
        i = 2  # skip scrutinee
        ncases = Int(imm_value(getop(ir, s, i))::Int64); i += 1
        for _ in 1:ncases
            i += 1  # skip case value (CONST/INLINE)
            push!(out, readbundle())
        end
        push!(out, readbundle())  # default
    elseif k === K"await"
        i = 2  # skip flags
        push!(out, readbundle())  # normal edge
        push!(out, readbundle())  # resume edge
    end
    @assert i - 1 <= n
    return out
end

function verify_floating_acyclic(ir::IR)
    # acyclicity modulo the delayed data edge (§4.3): DFS over STMT operands,
    # cutting operand 1 of delay-like kinds.
    body = ir.body
    n = Int(body.len)
    color = zeros(UInt8, n)   # 0 white, 1 gray, 2 black
    function visit(i::Int)
        color[i] == 2 && return
        color[i] == 1 && verr("floating: instantaneous dependency cycle through %$i")
        color[i] = 1
        s = StmtId(i)
        k = body.kind[i]
        cut = is_delay_kind(k) ? 1 : 0
        for j in 1:nops(ir, s)
            j == cut && continue
            o = getop(ir, s, j)
            optag(o) == TAG_STMT && visit(Int(payload(o)))
        end
        color[i] = 2
    end
    for i in 1:n
        body.kind[i] === KIND_DELETED && continue
        color[i] == 0 && visit(i)
    end
    return true
end
