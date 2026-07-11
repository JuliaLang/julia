# Textual format printer (§9): MLIR-flavored, versioned grammar. The portable
# subset round-trips against structural equality; unsupported constants print
# as opaque markers the parser rejects cleanly.

"Print `ir` to `io` (or return a String with no io argument)."
function print_ir(io::IO, ir::IR; name::Union{Nothing,Symbol} = nothing)
    fname = something(name, get(ir.meta, :name, :f))
    if layout(ir) === LAYOUT_FLOATING
        print_floating(io, ir, fname)
    else
        print_func(io, ir, fname)
    end
end
print_ir(ir::IR; kws...) = sprint(io -> print_ir(io, ir; kws...))

function print_func(io::IO, ir::IR, fname)
    root = getregion(ir, root_region(ir))
    print(io, "func @", fname, "(")
    join(io, ("%$(a.id)::$(type_str(stmt_type(ir, a)))" for a in root.args), ", ")
    print(io, ") -> ", type_str(get(ir.meta, :rettype, Any)), " {\n")
    print_region_body(io, ir, root_region(ir), 1; skipargs = true)
    print(io, "}\n")
end

function print_floating(io::IO, ir::IR, fname)
    root = getregion(ir, root_region(ir))
    print(io, "node @", fname, "(")
    join(io, ("%$(a.id)::$(type_str(stmt_type(ir, a)))" for a in root.args), ", ")
    print(io, ")  layout=floating {\n")
    # guard region declarations
    for (ri, reg) in enumerate(ir.regions)
        (is_guard(reg) && !reg.dead) || continue
        print(io, "  region ^g", ri, " = guard(", region_label(ir, reg.parent),
              ", cond ", reg.negated ? "!" : "", "%", reg.cond.id, ")\n")
    end
    for i in 1:Int(ir.body.len)
        ir.body.kind[i] === KIND_DELETED && continue
        ir.body.kind[i] === K"region_arg" && stmt_region(ir, StmtId(i)) == root_region(ir) && continue
        s = StmtId(i)
        print(io, "  eq ")
        r = stmt_region(ir, s)
        print(io, "%", i)
        r != root_region(ir) && print(io, " @^g", r.id)
        print(io, " = ", kindname(stmt_kind(ir, s)), " ")
        print_plain_operands(io, ir, s)
        print(io, " :: ", type_str(stmt_type(ir, s)), "\n")
    end
    print(io, "}\n")
end

region_label(ir::IR, r::RegionId) = r == root_region(ir) ? "^base" : "^g$(r.id)"

indent(io, n) = print(io, "  "^n)

function print_region_body(io::IO, ir::IR, r::RegionId, depth::Int; skipargs::Bool = false)
    for s in region_stmts(ir, r)
        skipargs && stmt_kind(ir, s) === K"region_arg" && continue
        print_stmt(io, ir, s, depth)
    end
end

function print_stmt(io::IO, ir::IR, s::StmtId, depth::Int)
    # optional per-statement annotation hook (e.g. source excerpts from a
    # provenance column, §3.7 Level 2): pass
    # IOContext(io, :stmt_annotate => (ir, s) -> "text or nothing")
    ann = get(io, :stmt_annotate, nothing)
    if ann !== nothing
        txt = ann(ir, s)
        if txt isa AbstractString && !isempty(txt)
            indent(io, depth)
            print(io, "// ", txt, "\n")
        end
    end
    k = stmt_kind(ir, s)
    indent(io, depth)
    if k === K"if"
        has_result_uses(ir, s) && print(io, "%", s.id, " = ")
        print(io, "if ", operand_str(ir, getop(ir, s, 1)), " :: ", type_str(stmt_type(ir, s)), " {\n")
        rs = live_owned_regions(ir, s)
        print_region_body(io, ir, rs[1], depth + 1)
        indent(io, depth); print(io, "}")
        if length(rs) >= 2
            print(io, " else {\n")
            print_region_body(io, ir, rs[2], depth + 1)
            indent(io, depth); print(io, "}")
        end
        print(io, "\n")
    elseif k === K"loop"
        print(io, "%", s.id, " = loop (")
        rs = live_owned_regions(ir, s)
        body = getregion(ir, rs[1])
        inits = operands(ir, s)
        join(io, ("init %$(a.id)::$(type_str(stmt_type(ir, a))) = $(operand_str(ir, inits[i]))"
                  for (i, a) in enumerate(body.args)), ", ")
        print(io, ") :: ", type_str(stmt_type(ir, s)), " {\n")
        print_region_body(io, ir, rs[1], depth + 1; skipargs = true)
        indent(io, depth); print(io, "}\n")
    elseif k === K"try"
        has_result_uses(ir, s) && print(io, "%", s.id, " = ")
        print(io, "try :: ", type_str(stmt_type(ir, s)), " {\n")
        rs = live_owned_regions(ir, s)
        print_region_body(io, ir, rs[1], depth + 1)
        indent(io, depth); print(io, "}")
        if length(rs) >= 2
            h = getregion(ir, rs[2])
            print(io, " catch (")
            join(io, ("%$(a.id)::$(type_str(stmt_type(ir, a)))" for a in h.args), ", ")
            print(io, ") {\n")
            print_region_body(io, ir, rs[2], depth + 1; skipargs = true)
            indent(io, depth); print(io, "}")
        end
        print(io, "\n")
    elseif k === K"cfg"
        has_result_uses(ir, s) && print(io, "%", s.id, " = ")
        print(io, "cfg (")
        join(io, (operand_str(ir, o) for o in operands(ir, s)), ", ")
        print(io, ") :: ", type_str(stmt_type(ir, s)), " {\n")
        for rid in live_owned_regions(ir, s)
            blk = getregion(ir, rid)
            indent(io, depth); print(io, "^bb", rid.id, "(")
            join(io, ("%$(a.id)::$(type_str(stmt_type(ir, a)))" for a in blk.args), ", ")
            print(io, "):\n")
            print_region_body(io, ir, rid, depth + 1; skipargs = true)
        end
        indent(io, depth); print(io, "}\n")
    elseif k === K"closure"
        print(io, "%", s.id, " = closure (")
        rs = live_owned_regions(ir, s)
        body = getregion(ir, rs[1])
        join(io, ("%$(a.id)::$(type_str(stmt_type(ir, a)))" for a in body.args), ", ")
        print(io, ") :: ", type_str(stmt_type(ir, s)), " {\n")
        print_region_body(io, ir, rs[1], depth + 1; skipargs = true)
        indent(io, depth); print(io, "}\n")
    elseif k === K"break"
        tgt = asregion(getop(ir, s, 1))
        print(io, "break %", getregion(ir, tgt).owner.id)
        nops(ir, s) > 1 && print(io, " (", join((operand_str(ir, getop(ir, s, i)) for i in 2:nops(ir, s)), ", "), ")")
        print(io, "\n")
    elseif k === K"continue"
        tgt = asregion(getop(ir, s, 1))
        print(io, "continue %", getregion(ir, tgt).owner.id,
              " if ", operand_str(ir, getop(ir, s, 2)))
        nops(ir, s) > 2 && print(io, " (", join((operand_str(ir, getop(ir, s, i)) for i in 3:nops(ir, s)), ", "), ")")
        print(io, "\n")
    elseif k === K"goto"
        print(io, "goto ")
        print_edge(io, ir, edge_bundles(ir, s)[1])
        print(io, "\n")
    elseif k === K"br_if"
        bs = edge_bundles(ir, s)
        print(io, "br_if ", operand_str(ir, getop(ir, s, 1)), " ")
        print_edge(io, ir, bs[1]); print(io, " ")
        print_edge(io, ir, bs[2]); print(io, "\n")
    elseif k === K"switch"
        bs = edge_bundles(ir, s)
        print(io, "switch ", operand_str(ir, getop(ir, s, 1)), " [")
        ncases = length(bs) - 1
        i = 3  # operand index of first case value
        parts = String[]
        opidx = 2
        opidx += 1 # ncases imm
        for c in 1:ncases
            caseval = operand_str(ir, getop(ir, s, opidx)); opidx += 1
            dest, args = bs[c]
            opidx += 2 + length(args)
            push!(parts, string(caseval, " -> ", sprint(io2 -> print_edge(io2, ir, bs[c]))))
        end
        push!(parts, string("default -> ", sprint(io2 -> print_edge(io2, ir, bs[end]))))
        join(io, parts, ", ")
        print(io, "]\n")
    elseif k === K"await"
        bs = edge_bundles(ir, s)
        print(io, "await flags=", operand_str(ir, getop(ir, s, 1)), " (normal ")
        print_edge(io, ir, bs[1])
        print(io, ") (resume ")
        print_edge(io, ir, bs[2])
        print(io, ")\n")
    elseif k === K"yield" || k === K"return" || k === K"unreachable"
        print(io, kindname(k))
        if nops(ir, s) > 0
            print(io, " ", join((operand_str(ir, getop(ir, s, i)) for i in 1:nops(ir, s)), ", "))
        end
        print(io, "\n")
    else
        # plain statement
        if result_arity(k) != 0
            print(io, "%", s.id, " = ")
        end
        print(io, kindname(k))
        nops(ir, s) > 0 && print(io, " ")
        print_plain_operands(io, ir, s)
        if result_arity(k) != 0
            print(io, " :: ", type_str(stmt_type(ir, s)))
        end
        f = stmt_flag(ir, s)
        f != kindinfo(k).effects && print(io, " !flag(0x", string(f; base = 16, pad = 8), ")")
        print(io, "\n")
    end
end

print_plain_operands(io::IO, ir::IR, s::StmtId) =
    join(io, (operand_str(ir, getop(ir, s, i)) for i in 1:nops(ir, s)), ", ")

function print_edge(io::IO, ir::IR, bundle::Tuple{RegionId,Vector{Operand}})
    dest, args = bundle
    print(io, "(^bb", dest.id)
    isempty(args) || print(io, ": ", join((operand_str(ir, a) for a in args), ", "))
    print(io, ")")
end

function has_result_uses(ir::IR, s::StmtId)
    result_arity(stmt_kind(ir, s)) != 0
end

function live_owned_regions(ir::IR, s::StmtId)
    [r for r in owned_regions(ir, s) if !getregion(ir, r).dead]
end

function operand_str(ir::IR, o::Operand)
    t = optag(o)
    if t == TAG_STMT
        return "%$(payload(o))"
    elseif t == TAG_INLINE
        return "const $(const_repr(imm_value(o)))"
    elseif t == TAG_CONST
        return "const $(const_repr(ir.body.constants[payload(o)]))"
    elseif t == TAG_GLOBAL
        g = ir.body.globals[payload(o)]
        return "global $(g.mod).$(g.name)"
    elseif t == TAG_SPARAM
        return "sparam $(payload(o))"
    elseif t == TAG_REGION
        return "^r$(payload(o))"
    elseif t == TAG_BLOCK
        return "^bb$(payload(o))"
    elseif t == TAG_NONE
        return "none"
    end
    return "<op?>"
end

function const_repr(@nospecialize(v))
    if v isa Int64
        return string(v)
    elseif v isa Bool
        return string(v)
    elseif v isa UInt8
        return "0x" * string(v; base = 16, pad = 2)
    elseif v isa Float64
        return repr(v)
    elseif v isa String
        return repr(v)
    elseif v isa Symbol
        return repr(v)
    elseif v === nothing
        return "nothing"
    elseif v isa Type
        return "type " * type_str(v)
    else
        # outside the portable subset: opaque marker (parser rejects cleanly)
        return "#<opaque $(typeof(v))>"
    end
end

function type_str(@nospecialize(t))
    t === nothing && return "?"
    if t isa Core.Const
        v = sprint(show, t.val; context = :limit => true)
        length(v) > 40 && (v = first(v, 39) * "\u2026")
        return "Const(" * v * ")"
    end
    if t isa Type
        if t === Union{}
            return "Union{}"
        elseif t isa Union
            parts = Any[]
            u = t
            while u isa Union
                push!(parts, u.a); u = u.b
            end
            push!(parts, u)
            return "Union{" * join(map(type_str, parts), ", ") * "}"
        elseif t isa DataType && t <: Tuple && t !== Tuple
            return "Tuple{" * join(map(type_str, collect(t.parameters)), ", ") * "}"
        else
            return string(nameof_safe(t))
        end
    end
    return "#<lattice $(typeof(t))>"
end

nameof_safe(@nospecialize(t)) = try
    t isa DataType ? (isempty(t.parameters) ? string(t) : string(t)) : string(t)
catch
    "?"
end

# ---------------------------------------------------------------------------
# Structural equality (round-trip comparison; distinct from object identity)
# ---------------------------------------------------------------------------

"""
    struct_eq(a::IR, b::IR) -> Bool

Structural equality over the portable subset: kinds, operand structure
(with constants compared by `==` and inline/pool encoding normalized),
types, flags, region tree shape.
"""
function struct_eq(a::IR, b::IR)
    nstmts(a) == nstmts(b) || return false
    length(a.regions) == length(b.regions) || return false
    for i in 1:nstmts(a)
        s = StmtId(i)
        stmt_kind(a, s) === stmt_kind(b, s) || return false
        stmt_kind(a, s) === KIND_DELETED && continue
        nops(a, s) == nops(b, s) || return false
        for j in 1:nops(a, s)
            oa, ob = getop(a, s, j), getop(b, s, j)
            opeq(a, oa, b, ob) || return false
        end
        ta, tb = stmt_type(a, s), stmt_type(b, s)
        (ta === tb || isequal(ta, tb)) || return false
        stmt_flag(a, s) == stmt_flag(b, s) || return false
        stmt_region(a, s) == stmt_region(b, s) || return false
    end
    for ri in 1:length(a.regions)
        ra, rb = a.regions[ri], b.regions[ri]
        (ra.kind === rb.kind && ra.activation === rb.activation &&
         ra.owner == rb.owner && ra.parent == rb.parent &&
         ra.args == rb.args && ra.negated == rb.negated &&
         ra.dead == rb.dead) || return false
        is_guard(ra) && (ra.cond == rb.cond || return false)
    end
    return true
end

function opeq(a::IR, oa::Operand, b::IR, ob::Operand)
    ta, tb = optag(oa), optag(ob)
    va = ta == TAG_CONST ? a.body.constants[payload(oa)] :
         ta == TAG_INLINE ? imm_value(oa) : nothing
    vb = tb == TAG_CONST ? b.body.constants[payload(ob)] :
         tb == TAG_INLINE ? imm_value(ob) : nothing
    if (ta == TAG_CONST || ta == TAG_INLINE) && (tb == TAG_CONST || tb == TAG_INLINE)
        return isequal(va, vb)
    end
    ta == tb || return false
    if ta == TAG_GLOBAL
        return a.body.globals[payload(oa)] == b.body.globals[payload(ob)]
    end
    return payload(oa) == payload(ob)
end

# REPL display: the full listing by default. Truncation is opt-in: set a
# line budget globally with `display_maxlines!(n)` or per-stream with
# `IOContext(io, :ir_maxlines => n)`. (The compact one-liner via 2-arg
# `show` stays for embedded contexts.)

"Global REPL display line budget for IR listings; 0 = unlimited (default)."
const DISPLAY_MAXLINES = Ref{Int}(0)

"""
    display_maxlines!(n::Union{Integer,Nothing})

Opt into truncated REPL display of IR listings: at most `n` lines, then a
tail note. `nothing` or `0` restores the default full listing. A per-stream
`IOContext(io, :ir_maxlines => n)` overrides the global setting.
"""
function display_maxlines!(n::Union{Integer,Nothing})
    DISPLAY_MAXLINES[] = n === nothing ? 0 : Int(n)
    return nothing
end

function Base.show(io::IO, ::MIME"text/plain", ir::IR)
    s = sprint(print_ir, ir)
    maxl = get(io, :ir_maxlines, DISPLAY_MAXLINES[])
    maxl = maxl isa Integer ? Int(maxl) : 0
    if maxl > 0
        lines = split(s, '\n')
        isempty(lines[end]) && pop!(lines)
        if length(lines) > maxl
            for l in @view lines[1:maxl]
                println(io, l)
            end
            print(io, "  \u22ee (", length(lines) - maxl, " more lines \u2014 ",
                  nstmts(ir), " stmts, ", nregions(ir),
                  " regions; `print_ir(stdout, ir)` for the full listing)")
            return nothing
        end
    end
    print(io, s)
    return nothing
end
