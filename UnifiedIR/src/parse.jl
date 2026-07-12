# Textual format parser (portable subset, §9). Rejects opaque markers and
# unsupported constants cleanly rather than claiming a false round trip.

mutable struct Lexer
    s::String
    pos::Int
end

struct ParseError <: Exception
    msg::String
end
Base.showerror(io::IO, e::ParseError) = print(io, "ParseError: ", e.msg)
perr(msg...) = throw(ParseError(string(msg...)))

function skipws!(lx::Lexer)
    while lx.pos <= ncodeunits(lx.s)
        c = lx.s[lx.pos]
        if c in (' ', '\t', '\n', '\r', ';')
            lx.pos = nextind(lx.s, lx.pos)
        elseif c == '#' && lx.pos + 1 <= ncodeunits(lx.s) && lx.s[lx.pos+1] == '#'
            while lx.pos <= ncodeunits(lx.s) && lx.s[lx.pos] != '\n'
                lx.pos = nextind(lx.s, lx.pos)
            end
        else
            break
        end
    end
end

function peekch(lx::Lexer)
    skipws!(lx)
    lx.pos > ncodeunits(lx.s) && return '\0'
    return lx.s[lx.pos]
end

function matchtok!(lx::Lexer, t::String)
    skipws!(lx)
    if startswith(SubString(lx.s, lx.pos), t)
        # identifiers must not run on
        if isletter(t[end]) || t[end] == '_'
            nxt = lx.pos + ncodeunits(t)
            if nxt <= ncodeunits(lx.s)
                c = lx.s[nxt]
                (isletter(c) || isdigit(c) || c == '_' || c == '.') && return false
            end
        end
        lx.pos += ncodeunits(t)
        return true
    end
    return false
end

expect!(lx::Lexer, t::String) = matchtok!(lx, t) || perr("expected `$t` at …", context(lx))
context(lx::Lexer) = repr(SubString(lx.s, lx.pos, min(ncodeunits(lx.s), lx.pos + 40)))

# Raw prefix match with no identifier-boundary rule: for label prefixes that a
# number immediately follows (`^bb3`, `^g2`), where matchtok!'s boundary check
# would otherwise reject the token.
function match_raw!(lx::Lexer, t::String)
    skipws!(lx)
    startswith(SubString(lx.s, lx.pos), t) || return false
    lx.pos += ncodeunits(t)
    return true
end
expect_raw!(lx::Lexer, t::String) = match_raw!(lx, t) || perr("expected `$t` at …", context(lx))

function ident!(lx::Lexer)
    skipws!(lx)
    start = lx.pos
    while lx.pos <= ncodeunits(lx.s)
        c = lx.s[lx.pos]
        (isletter(c) || isdigit(c) || c == '_' || c == '.' || c == '!') || break
        lx.pos = nextind(lx.s, lx.pos)
    end
    lx.pos > start || perr("expected identifier at ", context(lx))
    return SubString(lx.s, start, prevind(lx.s, lx.pos))
end

function number!(lx::Lexer)
    skipws!(lx)
    start = lx.pos
    lx.pos <= ncodeunits(lx.s) && lx.s[lx.pos] == '-' && (lx.pos += 1)
    if startswith(SubString(lx.s, lx.pos), "0x")
        lx.pos += 2
        while lx.pos <= ncodeunits(lx.s) && (isdigit(lx.s[lx.pos]) || lx.s[lx.pos] in 'a':'f')
            lx.pos += 1
        end
        return parse(UInt64, SubString(lx.s, start, lx.pos - 1))
    end
    isfloat = false
    while lx.pos <= ncodeunits(lx.s)
        c = lx.s[lx.pos]
        if isdigit(c)
            lx.pos += 1
        elseif c == '.' && lx.pos + 1 <= ncodeunits(lx.s) && isdigit(lx.s[lx.pos+1])
            isfloat = true; lx.pos += 1
        elseif c in ('e', 'E') && isfloat
            lx.pos += 1
            lx.pos <= ncodeunits(lx.s) && lx.s[lx.pos] in ('+', '-') && (lx.pos += 1)
        else
            break
        end
    end
    str = SubString(lx.s, start, lx.pos - 1)
    isempty(str) && perr("expected number at ", context(lx))
    return isfloat ? parse(Float64, str) : parse(Int64, str)
end

function pct_id!(lx::Lexer)
    expect!(lx, "%")
    n = number!(lx)
    n isa Int64 || perr("bad %id")
    return Int(n)
end

# ---------------------------------------------------------------------------

const TYPE_TABLE = Dict{String,Any}(
    "Any" => Any, "Nothing" => Nothing, "Bool" => Bool,
    "Int" => Int, "Int8" => Int8, "Int16" => Int16, "Int32" => Int32, "Int64" => Int64,
    "UInt8" => UInt8, "UInt16" => UInt16, "UInt32" => UInt32, "UInt64" => UInt64,
    "Float32" => Float32, "Float64" => Float64,
    "String" => String, "Symbol" => Symbol, "Char" => Char,
    "?" => nothing)

function parse_type!(lx::Lexer)
    if matchtok!(lx, "?")
        return nothing
    end
    name = String(ident!(lx))
    if name == "Tuple" || name == "Union"
        expect!(lx, "{")
        params = Any[]
        if !matchtok!(lx, "}")
            while true
                push!(params, parse_type!(lx))
                matchtok!(lx, ",") || break
            end
            expect!(lx, "}")
        end
        return name == "Tuple" ? Tuple{params...} : Union{params...}
    end
    haskey(TYPE_TABLE, name) && return TYPE_TABLE[name]
    perr("type `$name` is outside the portable subset")
end

function parse_const!(lx::Lexer, mod::Module)
    skipws!(lx)
    c = peekch(lx)
    if c == '"'
        # string literal
        lx.pos += 1
        buf = IOBuffer()
        while lx.pos <= ncodeunits(lx.s) && lx.s[lx.pos] != '"'
            ch = lx.s[lx.pos]
            if ch == '\\'
                lx.pos += 1
                esc = lx.s[lx.pos]
                write(buf, esc == 'n' ? '\n' : esc == 't' ? '\t' : esc)
            else
                write(buf, ch)
            end
            lx.pos = nextind(lx.s, lx.pos)
        end
        expect!(lx, "\"")
        return String(take!(buf))
    elseif c == ':'
        lx.pos += 1
        return Symbol(ident!(lx))
    elseif c == '#'
        perr("opaque constant marker: outside the portable subset")
    elseif isdigit(c) || c == '-'
        return number!(lx)
    else
        w = ident!(lx)
        w == "true" && return true
        w == "false" && return false
        w == "nothing" && return nothing
        w == "type" && return parse_type!(lx)
        perr("bad constant `$w`")
    end
end

# ---------------------------------------------------------------------------
# Statement-level parsing
# ---------------------------------------------------------------------------

mutable struct PCtx
    b::Builder
    idmap::Dict{Int,StmtId}              # textual %N -> actual
    blockmap::Dict{Int,RegionId}         # textual ^bbN -> actual (per cfg)
    blockfix::Vector{Tuple{StmtId,Int,Int}}  # (stmt, opidx, textual bb)
    mod::Module
    allow_forward::Bool                  # floating nodes: %N may be forward
    stmtfix::Vector{Tuple{StmtId,Int,Int}}   # (stmt, opidx, textual %N)
end
PCtx(b, idmap, blockmap, blockfix, mod) =
    PCtx(b, idmap, blockmap, blockfix, mod, false, Tuple{StmtId,Int,Int}[])

function parse_operand!(lx::Lexer, ctx::PCtx)
    skipws!(lx)
    c = peekch(lx)
    if c == '%'
        n = pct_id!(lx)
        if !haskey(ctx.idmap, n)
            ctx.allow_forward || perr("undefined %$n")
            return (:stmtref, n)         # deferred; resolved by stmt fixups
        end
        return op_stmt(ctx.idmap[n])
    elseif matchtok!(lx, "const")
        v = parse_const!(lx, ctx.mod)
        return vop(ctx.b.ir, v)
    elseif matchtok!(lx, "global")
        path = String(ident!(lx))
        parts = split(path, '.')
        m = ctx.mod
        root = Symbol(parts[1])
        if root === :Base
            m = Base
        elseif root === :Core
            m = Core
        elseif root === :Main
            m = Main
        else
            m = getglobal(ctx.mod, root)::Module
        end
        for p in parts[2:end-1]
            m = getglobal(m, Symbol(p))::Module
        end
        return vop(ctx.b.ir, GlobalRef(m, Symbol(parts[end])))
    elseif matchtok!(lx, "sparam")
        n = number!(lx)
        return op_sparam(Int(n))
    elseif matchtok!(lx, "none")
        return OP_NONE
    elseif c == '^'
        lx.pos += 1
        if match_raw!(lx, "bb")
            n = number!(lx)
            return (:blockref, Int(n))   # resolved by caller
        else
            perr("unexpected region reference")
        end
    else
        perr("bad operand at ", context(lx))
    end
end

value_operand!(lx, ctx) = begin
    o = parse_operand!(lx, ctx)
    o isa Operand || perr("block reference where value expected")
    o
end

function parse_operand_list!(lx::Lexer, ctx::PCtx)
    ops = Operand[]
    skipws!(lx)
    while true
        c = peekch(lx)
        (c in ('%', '^') || isletter(c)) || break
        # stop words that start structured forms
        save = lx.pos
        if isletter(c)
            w = ident!(lx)
            lx.pos = save
            w in ("const", "global", "sparam", "none") || break
        end
        push!(ops, value_operand!(lx, ctx))
        matchtok!(lx, ",") || break
    end
    return ops
end

"Parse the textual format produced by `print_ir`."
function parse_ir(src::AbstractString; mod::Module = Main, cols = NOCOLS)
    lx = Lexer(String(src), 1)
    if matchtok!(lx, "func")
        return parse_func!(lx, mod, cols)
    elseif matchtok!(lx, "node")
        return parse_node!(lx, mod, cols)
    else
        perr("expected `func` or `node`")
    end
end

function parse_func!(lx::Lexer, mod::Module, cols)
    expect!(lx, "@")
    fname = Symbol(ident!(lx))
    b = Builder(; cols, name = fname)
    ctx = PCtx(b, Dict{Int,StmtId}(), Dict{Int,RegionId}(), Tuple{StmtId,Int,Int}[], mod)
    expect!(lx, "(")
    if !matchtok!(lx, ")")
        while true
            n = pct_id!(lx)
            expect!(lx, "::")
            t = parse_type!(lx)
            s = append_stmt!(b, K"region_arg"; type = t)
            ctx.idmap[n] = s
            push!(b.ir.argtypes, t)
            matchtok!(lx, ",") || break
        end
        expect!(lx, ")")
    end
    expect!(lx, "->")
    b.ir.meta[:rettype] = parse_type!(lx)
    expect!(lx, "{")
    parse_stmts!(lx, ctx)
    expect!(lx, "}")
    resolve_blockfix!(ctx)
    return finish!(b)
end

function resolve_blockfix!(ctx::PCtx)
    for (s, opidx, bb) in ctx.blockfix
        haskey(ctx.blockmap, bb) || perr("undefined block ^bb$bb")
        w = ctx.b.ir.body.ops[s.id]
        @assert !is_ops_inline(w)
        ctx.b.ir.body.operands[ops_offset(w) + opidx] = op_block(ctx.blockmap[bb])
    end
    empty!(ctx.blockfix)
end

function parse_stmts!(lx::Lexer, ctx::PCtx)
    while true
        skipws!(lx)
        c = peekch(lx)
        (c == '}' || c == '\0' || c == '^') && return
        parse_stmt!(lx, ctx)
    end
end

function parse_stmt!(lx::Lexer, ctx::PCtx)
    b = ctx.b
    textid = 0
    if peekch(lx) == '%'
        textid = pct_id!(lx)
        expect!(lx, "=")
    end
    save = lx.pos
    w = String(ident!(lx))
    if w == "if"
        cond = value_operand!(lx, ctx)
        s = append_stmt!(b, K"if", cond; type = Any)
        textid != 0 && (ctx.idmap[textid] = s)
        expect!(lx, "{")
        open_region!(b, s; kind = REGION_ARM)
        parse_stmts!(lx, ctx); close_region!(b)
        expect!(lx, "}")
        if matchtok!(lx, "else")
            expect!(lx, "{")
            open_region!(b, s; kind = REGION_ARM)
            parse_stmts!(lx, ctx); close_region!(b)
            expect!(lx, "}")
        end
        expect!(lx, "::")
        set_type!(b.ir, s, parse_type!(lx))
    elseif w == "loop"
        expect!(lx, "(")
        argdecls = Tuple{Int,Any,Operand}[]
        if !matchtok!(lx, ")")
            while true
                expect!(lx, "init")
                n = pct_id!(lx)
                expect!(lx, "::")
                t = parse_type!(lx)
                expect!(lx, "=")
                init = value_operand!(lx, ctx)
                push!(argdecls, (n, t, init))
                matchtok!(lx, ",") || break
            end
            expect!(lx, ")")
        end
        s = append_stmt!(b, K"loop", (d[3] for d in argdecls)...; type = Any)
        textid != 0 && (ctx.idmap[textid] = s)
        expect!(lx, "{")
        open_region!(b, s; kind = REGION_LOOP_BODY)
        for (n, at, _) in argdecls
            a = append_stmt!(b, K"region_arg"; type = at)
            ctx.idmap[n] = a
        end
        parse_stmts!(lx, ctx); close_region!(b)
        expect!(lx, "}")
        expect!(lx, "::")
        set_type!(b.ir, s, parse_type!(lx))
    elseif w == "try"
        s = append_stmt!(b, K"try"; type = Any)
        textid != 0 && (ctx.idmap[textid] = s)
        expect!(lx, "{")
        open_region!(b, s; kind = REGION_BODY)
        parse_stmts!(lx, ctx); close_region!(b)
        expect!(lx, "}")
        if matchtok!(lx, "catch")
            expect!(lx, "(")
            open_region!(b, s; kind = REGION_HANDLER)
            if !matchtok!(lx, ")")
                while true
                    n = pct_id!(lx)
                    expect!(lx, "::")
                    at = parse_type!(lx)
                    a = append_stmt!(b, K"region_arg"; type = at)
                    ctx.idmap[n] = a
                    matchtok!(lx, ",") || break
                end
                expect!(lx, ")")
            end
            expect!(lx, "{")
            parse_stmts!(lx, ctx); close_region!(b)
            expect!(lx, "}")
        end
        expect!(lx, "::")
        set_type!(b.ir, s, parse_type!(lx))
    elseif w == "cfg"
        expect!(lx, "(")
        entryops = Operand[]
        if !matchtok!(lx, ")")
            while true
                push!(entryops, value_operand!(lx, ctx))
                matchtok!(lx, ",") || break
            end
            expect!(lx, ")")
        end
        s = append_stmt!(b, K"cfg", entryops...; type = Any)
        textid != 0 && (ctx.idmap[textid] = s)
        expect!(lx, "{")
        oldblocks = copy(ctx.blockmap)
        empty!(ctx.blockmap)
        while peekch(lx) == '^'
            expect_raw!(lx, "^bb")
            bbn = Int(number!(lx))
            rid = open_region!(b, s; kind = REGION_BLOCK)
            ctx.blockmap[bbn] = rid
            expect!(lx, "(")
            if !matchtok!(lx, ")")
                while true
                    n = pct_id!(lx)
                    expect!(lx, "::")
                    at = parse_type!(lx)
                    a = append_stmt!(b, K"region_arg"; type = at)
                    ctx.idmap[n] = a
                    matchtok!(lx, ",") || break
                end
                expect!(lx, ")")
            end
            expect!(lx, ":")
            parse_stmts!(lx, ctx)
            close_region!(b)
        end
        expect!(lx, "}")
        expect!(lx, "::")
        set_type!(b.ir, s, parse_type!(lx))
        resolve_blockfix!(ctx)
        empty!(ctx.blockmap)
        merge!(ctx.blockmap, oldblocks)
    elseif w == "break" || w == "continue"
        ownern = pct_id!(lx)
        owner = ctx.idmap[ownern]
        body = nothing
        for r in owned_regions(b.ir, owner)
            getregion(b.ir, r).kind === REGION_LOOP_BODY && (body = r)
        end
        body === nothing && perr("`$w` target %$ownern is not a loop")
        ops = Operand[op_region(body)]
        if w == "continue"
            expect!(lx, "if")
            push!(ops, value_operand!(lx, ctx))
        end
        if matchtok!(lx, "(")
            if !matchtok!(lx, ")")
                while true
                    push!(ops, value_operand!(lx, ctx))
                    matchtok!(lx, ",") || break
                end
                expect!(lx, ")")
            end
        end
        append_stmt!(b, w == "break" ? K"break" : K"continue", ops...; type = Nothing)
    elseif w == "result" || w == "return"
        ops = parse_operand_list!(lx, ctx)
        append_stmt!(b, w == "result" ? K"result" : K"return", ops...; type = Nothing)
    elseif w == "unreachable"
        append_stmt!(b, K"unreachable"; type = Nothing)
    elseif w == "goto"
        ops = Operand[]
        parse_edge!(lx, ctx, ops)
        s = append_stmt!(b, K"goto", ops...; type = Nothing)
        shift_blockfix!(ctx, s, 0)
    elseif w == "br_if"
        cond = value_operand!(lx, ctx)
        ops = Operand[cond]
        parse_edge!(lx, ctx, ops)
        parse_edge!(lx, ctx, ops)
        s = append_stmt!(b, K"br_if", ops...; type = Nothing)
        shift_blockfix!(ctx, s, 0)
    elseif w == "switch"
        scrut = value_operand!(lx, ctx)
        ops = Operand[scrut, op_inline(0)]
        ncases = 0
        expect!(lx, "[")
        while !matchtok!(lx, "default")
            matchtok!(lx, "const")   # printed case values carry the const prefix
            v = parse_const!(lx, ctx.mod)
            expect!(lx, "->")
            push!(ops, vop(b.ir, v))
            parse_edge!(lx, ctx, ops)
            ncases += 1
            matchtok!(lx, ",")
        end
        expect!(lx, "->")
        parse_edge!(lx, ctx, ops)
        expect!(lx, "]")
        ops[2] = op_inline(ncases)
        s = append_stmt!(b, K"switch", ops...; type = Nothing)
        shift_blockfix!(ctx, s, 0)
    elseif w == "await"
        expect!(lx, "flags"); expect!(lx, "=")
        flags = value_operand!(lx, ctx)
        ops = Operand[flags]
        expect!(lx, "("); expect!(lx, "normal")
        parse_edge!(lx, ctx, ops)
        expect!(lx, ")")
        expect!(lx, "("); expect!(lx, "resume")
        parse_edge!(lx, ctx, ops)
        expect!(lx, ")")
        s = append_stmt!(b, K"await", ops...; type = Nothing)
        shift_blockfix!(ctx, s, 0)
    else
        # plain statement: kind name + operands [:: type] [!flag(0x…)]
        k = try
            lookup_kind(w)
        catch
            lx.pos = save
            perr("unknown statement kind `$w`")
        end
        ops = parse_operand_list!(lx, ctx)
        t = Any
        if matchtok!(lx, "::")
            t = parse_type!(lx)
        end
        flag = nothing
        if matchtok!(lx, "!flag")
            expect!(lx, "(")
            f = number!(lx)
            expect!(lx, ")")
            flag = UInt32(f)
        end
        s = append_stmt!(b, k, ops...; type = t,
                         flag = flag === nothing ? nothing : flag)
        textid != 0 && (ctx.idmap[textid] = s)
    end
    return nothing
end

# Edge bundle: (^bbN[: ops...]) — pushes BLOCK placeholder + argc + args onto
# `ops`, recording a fixup for the block reference (blocks may be forward).
function parse_edge!(lx::Lexer, ctx::PCtx, ops::Vector{Operand})
    expect!(lx, "(")
    expect_raw!(lx, "^bb")
    bbn = Int(number!(lx))
    bidx = length(ops) + 1
    push!(ops, op_block(RegionId(0)))    # placeholder
    args = Operand[]
    if matchtok!(lx, ":")
        while true
            push!(args, value_operand!(lx, ctx))
            matchtok!(lx, ",") || break
        end
    end
    expect!(lx, ")")
    push!(ops, op_inline(length(args)))
    append!(ops, args)
    push!(ctx.blockfix, (NULL_STMT, bidx, bbn))  # stmt patched by shift_blockfix!
    return ops
end

# After the stmt is appended, bind the pending fixups (created with NULL_STMT)
# to it.
function shift_blockfix!(ctx::PCtx, s::StmtId, _)
    for i in 1:length(ctx.blockfix)
        if ctx.blockfix[i][1] == NULL_STMT
            ctx.blockfix[i] = (s, ctx.blockfix[i][2], ctx.blockfix[i][3])
        end
    end
end

# ---------------------------------------------------------------------------
# Floating nodes
# ---------------------------------------------------------------------------

function parse_node!(lx::Lexer, mod::Module, cols)
    expect!(lx, "@")
    fname = Symbol(ident!(lx))
    b = Builder(; cols, name = fname)
    ctx = PCtx(b, Dict{Int,StmtId}(), Dict{Int,RegionId}(), Tuple{StmtId,Int,Int}[], mod)
    ctx.allow_forward = true
    expect!(lx, "(")
    if !matchtok!(lx, ")")
        while true
            n = pct_id!(lx)
            expect!(lx, "::")
            t = parse_type!(lx)
            s = append_stmt!(b, K"region_arg"; type = t)
            ctx.idmap[n] = s
            push!(b.ir.argtypes, t)
            matchtok!(lx, ",") || break
        end
        expect!(lx, ")")
    end
    expect!(lx, "layout=floating")
    expect!(lx, "{")
    guardfix = Tuple{RegionId,Int}[]     # (region, textual cond id)
    guardmap = Dict{Int,RegionId}()
    while true
        skipws!(lx)
        peekch(lx) == '}' && break
        if matchtok!(lx, "region")
            expect_raw!(lx, "^g")
            gn = Int(number!(lx))
            expect!(lx, "="); expect!(lx, "guard"); expect!(lx, "(")
            parent = root_region(b.ir)
            if matchtok!(lx, "^")
                if matchtok!(lx, "base")
                    parent = root_region(b.ir)
                else
                    expect_raw!(lx, "g")
                    pn = Int(number!(lx))
                    parent = guardmap[pn]
                end
            end
            expect!(lx, ","); expect!(lx, "cond")
            neg = matchtok!(lx, "!")
            cn = pct_id!(lx)
            expect!(lx, ")")
            reg = Region(REGION_GUARD, NULL_STMT, parent; negated = neg)
            push!(b.ir.regions, reg)
            rid = RegionId(length(b.ir.regions))
            guardmap[gn] = rid
            push!(guardfix, (rid, cn))
        elseif matchtok!(lx, "eq")
            n = pct_id!(lx)
            target = root_region(b.ir)
            if matchtok!(lx, "@")
                expect_raw!(lx, "^g")
                gn = Int(number!(lx))
                target = guardmap[gn]
            end
            expect!(lx, "=")
            w = String(ident!(lx))
            k = lookup_kind(w)
            rawops = Any[]
            skipws!(lx)
            while true
                c2 = peekch(lx)
                (c2 in ('%', '^') || isletter(c2)) || break
                if isletter(c2)
                    save2 = lx.pos
                    w2 = ident!(lx)
                    lx.pos = save2
                    w2 in ("const", "global", "sparam", "none") || break
                end
                push!(rawops, parse_operand!(lx, ctx))
                matchtok!(lx, ",") || break
            end
            ops = Operand[]
            pend = Tuple{Int,Int}[]   # (opidx, textual id)
            for (i, o) in enumerate(rawops)
                if o isa Operand
                    push!(ops, o)
                else
                    o[1] === :stmtref || perr("bad operand in eq")
                    push!(ops, op_stmt(StmtId(0)))
                    push!(pend, (i, o[2]))
                end
            end
            t = Any
            matchtok!(lx, "::") && (t = parse_type!(lx))
            s = append_stmt!(b, k, ops...; type = t)
            b.ir.body.region[s.id] = target
            ctx.idmap[n] = s
            for (opidx, textn) in pend
                push!(ctx.stmtfix, (s, opidx, textn))
            end
        else
            perr("expected `region` or `eq` in node body")
        end
    end
    expect!(lx, "}")
    for (rid, cn) in guardfix
        haskey(ctx.idmap, cn) || perr("guard condition %$cn undefined")
        getregion(b.ir, rid).cond = ctx.idmap[cn]
    end
    # resolve forward statement references
    for (s, opidx, textn) in ctx.stmtfix
        haskey(ctx.idmap, textn) || perr("undefined %$textn (forward reference never defined)")
        tgt = ctx.idmap[textn]
        w = b.ir.body.ops[s.id]
        if is_ops_inline(w)
            opidx == 1 || perr("bad inline fixup")
            b.ir.body.ops[s.id] = set_inline_stmt(w, tgt)
        else
            b.ir.body.operands[ops_offset(w) + opidx] = op_stmt(tgt)
        end
    end
    empty!(ctx.stmtfix)
    b.ir.meta[:floating_node] = true
    ir = finish!(b; verify = false)
    ir.owner.state = LAYOUT_FLOATING
    verify_ir(ir; level = 0)
    return ir
end
