# A reference interpreter for dense sealed IR: the test dialect plus the core
# structural ops (if/loop/try/cfg/cells/extract/select). `call` applies
# constant/global callees, so Julia-flavored smoke tests run too. This is the
# execution半 of the differential harness for IR-only tests.

mutable struct CellBox
    defined::Bool
    value::Any
    CellBox() = new(false, nothing)
end

struct InterpResult
    kind::Symbol           # :return | :result | :break | :continue | :fallout
    target::RegionId       # for :break/:continue
    values::Vector{Any}
end

"""
    interpret(ir, args...; io=stdout) -> value

Execute a dense, sealed function body with the given arguments.
"""
function interpret(ir::IR, args...; io::IO = stdout)
    check_state(ir, LAYOUT_DENSE, "interpret")
    env = Vector{Any}(undef, Int(ir.body.len))
    root = getregion(ir, root_region(ir))
    length(args) == length(root.args) ||
        error("interpret: expected $(length(root.args)) arguments")
    for (i, a) in enumerate(root.args)
        env[a.id] = args[i]
    end
    res = run_region!(ir, env, root_region(ir), io)
    res.kind === :return && return isempty(res.values) ? nothing :
        length(res.values) == 1 ? res.values[1] : Tuple(res.values)
    res.kind === :fallout && return nothing
    error("interpret: unhandled outcome $(res.kind) at function scope")
end

function opval(ir::IR, env::Vector{Any}, o::Operand)
    t = optag(o)
    t == TAG_STMT && return env[payload(o)]
    t == TAG_INLINE && return imm_value(o)
    t == TAG_CONST && return ir.body.constants[payload(o)]
    t == TAG_GLOBAL && begin
        g = ir.body.globals[payload(o)]
        return getglobal(g.mod, g.name)
    end
    t == TAG_SPARAM && return ir.sptypes[payload(o)]
    error("interpret: cannot evaluate operand tag $t")
end

function run_region!(ir::IR, env::Vector{Any}, r::RegionId, io::IO)::InterpResult
    for s in region_stmts(ir, r)
        k = stmt_kind(ir, s)
        k === K"region_arg" && continue
        if is_terminator(k) || owns_regions(k)
            res = exec_control!(ir, env, s, io)
            res === nothing || return res
        else
            exec_plain!(ir, env, s, io)
        end
    end
    return InterpResult(:fallout, NULL_REGION, Any[])
end

vals(ir, env, s, from::Int) = Any[opval(ir, env, getop(ir, s, i)) for i in from:nops(ir, s)]

function exec_control!(ir::IR, env::Vector{Any}, s::StmtId, io::IO)::Union{Nothing,InterpResult}
    k = stmt_kind(ir, s)
    if k === K"if"
        c = opval(ir, env, getop(ir, s, 1))::Bool
        rs = live_owned_regions(ir, s)
        res = if c
            run_region!(ir, env, rs[1], io)
        elseif length(rs) >= 2
            run_region!(ir, env, rs[2], io)
        else
            InterpResult(:fallout, NULL_REGION, Any[])
        end
        if res.kind === :result
            bind_result!(env, s, res.values)
            return nothing
        elseif res.kind === :fallout
            env[s.id] = nothing
            return nothing
        else
            return res
        end
    elseif k === K"loop"
        rs = live_owned_regions(ir, s)
        bodyr = rs[1]
        breg = getregion(ir, bodyr)
        carried = vals(ir, env, s, 1)
        while true
            for (i, a) in enumerate(breg.args)
                env[a.id] = carried[i]
            end
            res = run_region!(ir, env, bodyr, io)
            if res.kind === :continue && res.target == bodyr
                cond = res.values[1]::Bool
                nextvals = res.values[2:end]
                if cond
                    carried = nextvals
                    continue
                else
                    bind_result!(env, s, nextvals)
                    return nothing
                end
            elseif res.kind === :break && res.target == bodyr
                bind_result!(env, s, res.values)
                return nothing
            elseif res.kind === :fallout
                error("interpret: loop body fell out without continue/break")
            else
                return res
            end
        end
    elseif k === K"try"
        rs = live_owned_regions(ir, s)
        res = try
            run_region!(ir, env, rs[1], io)
        catch exc
            length(rs) >= 2 || rethrow()
            h = getregion(ir, rs[2])
            isempty(h.args) || (env[h.args[1].id] = exc)
            run_region!(ir, env, rs[2], io)
        end
        if res.kind === :result
            bind_result!(env, s, res.values)
            return nothing
        elseif res.kind === :fallout
            env[s.id] = nothing
            return nothing
        else
            return res
        end
    elseif k === K"cfg"
        rs = live_owned_regions(ir, s)
        cur = rs[1]
        curargs = vals(ir, env, s, 1)
        while true
            blk = getregion(ir, cur)
            for (i, a) in enumerate(blk.args)
                env[a.id] = curargs[i]
            end
            res = run_region!(ir, env, cur, io)
            if res.kind === :goto
                if getregion(ir, res.target).owner != s
                    return res            # sealed cross-island exit (§5.5)
                end
                cur = res.target
                curargs = res.values
            elseif res.kind === :result
                bind_result!(env, s, res.values)
                return nothing
            elseif res.kind === :fallout
                error("interpret: cfg block fell out without a terminator")
            else
                return res
            end
        end
    elseif k === K"result"
        return InterpResult(:result, NULL_REGION, vals(ir, env, s, 1))
    elseif k === K"return"
        return InterpResult(:return, NULL_REGION, vals(ir, env, s, 1))
    elseif k === K"break"
        return InterpResult(:break, asregion(getop(ir, s, 1)), vals(ir, env, s, 2))
    elseif k === K"continue"
        return InterpResult(:continue, asregion(getop(ir, s, 1)), vals(ir, env, s, 2))
    elseif k === K"unreachable"
        error("interpret: reached unreachable")
    elseif k === K"goto"
        dest, args = edge_bundles(ir, s)[1]
        return InterpResult(:goto, dest, Any[opval(ir, env, a) for a in args])
    elseif k === K"br_if"
        c = opval(ir, env, getop(ir, s, 1))::Bool
        bs = edge_bundles(ir, s)
        dest, args = c ? bs[1] : bs[2]
        return InterpResult(:goto, dest, Any[opval(ir, env, a) for a in args])
    elseif k === K"switch"
        v = opval(ir, env, getop(ir, s, 1))
        bs = edge_bundles(ir, s)
        ncases = Int(imm_value(getop(ir, s, 2))::Int64)
        opidx = 3
        chosen = bs[end]
        for c in 1:ncases
            caseval = opval(ir, env, getop(ir, s, opidx))
            if isequal(v, caseval)
                chosen = bs[c]
                break
            end
            # advance past: caseval + BLOCK + argc + args
            opidx += 1 + 2 + length(bs[c][2])
        end
        dest, args = chosen
        return InterpResult(:goto, dest, Any[opval(ir, env, a) for a in args])
    elseif k === K"await"
        error("interpret: await requires a task runtime (not in the v1 interpreter)")
    elseif k === K"closure"
        error("interpret: closure execution enters at P3")
    else
        error("interpret: unhandled control kind $(kindname(k))")
    end
end

function bind_result!(env::Vector{Any}, s::StmtId, values::Vector{Any})
    env[s.id] = isempty(values) ? nothing :
        length(values) == 1 ? values[1] : Tuple(values)
end

function exec_plain!(ir::IR, env::Vector{Any}, s::StmtId, io::IO)
    k = stmt_kind(ir, s)
    q = kindname(k)
    if q === Symbol("test.iconst")
        env[s.id] = Int64(opval(ir, env, getop(ir, s, 1)))
    elseif q === Symbol("test.add")
        env[s.id] = opval(ir, env, getop(ir, s, 1)) + opval(ir, env, getop(ir, s, 2))
    elseif q === Symbol("test.mul")
        env[s.id] = opval(ir, env, getop(ir, s, 1)) * opval(ir, env, getop(ir, s, 2))
    elseif q === Symbol("test.icmp")
        pred = opval(ir, env, getop(ir, s, 1))::Symbol
        a = opval(ir, env, getop(ir, s, 2))
        b = opval(ir, env, getop(ir, s, 3))
        env[s.id] = pred === :sgt ? (a > b) :
                    pred === :slt ? (a < b) :
                    pred === :sge ? (a >= b) :
                    pred === :sle ? (a <= b) :
                    pred === :eq  ? (a == b) :
                    pred === :ne  ? (a != b) :
                    error("test.icmp: unknown predicate $pred")
    elseif q === Symbol("test.print")
        println(io, opval(ir, env, getop(ir, s, 1)))
    elseif q === Symbol("test.opaque")
        env[s.id] = nops(ir, s) >= 1 ? opval(ir, env, getop(ir, s, 1)) : nothing
    elseif q === Symbol("test.delay")
        error("interpret: test.delay must be legalized before dense execution")
    elseif k === K"extract"
        v = opval(ir, env, getop(ir, s, 1))
        i = Int(imm_value(getop(ir, s, 2))::Int64)
        env[s.id] = v[i]
    elseif k === K"select"
        c = opval(ir, env, getop(ir, s, 1))::Bool
        env[s.id] = c ? opval(ir, env, getop(ir, s, 2)) : opval(ir, env, getop(ir, s, 3))
    elseif k === K"refine"
        env[s.id] = opval(ir, env, getop(ir, s, 1))
    elseif k === K"value"
        env[s.id] = opval(ir, env, getop(ir, s, 1))
    elseif k === K"call"
        f = opval(ir, env, getop(ir, s, 1))
        env[s.id] = f(vals(ir, env, s, 2)...)
    elseif k === K"cell" || k === K"cell_shared"
        env[s.id] = CellBox()
    elseif k === K"cell_set"
        c = env[payload(getop(ir, s, 1))]::CellBox
        c.value = opval(ir, env, getop(ir, s, 2))
        c.defined = true
    elseif k === K"cell_get"
        c = env[payload(getop(ir, s, 1))]::CellBox
        c.defined || error("interpret: read of undefined cell (%$(s.id))")
        env[s.id] = c.value
    elseif k === K"cell_new"
        c = env[payload(getop(ir, s, 1))]::CellBox
        c.defined = false
        c.value = nothing
    elseif k === K"cell_isdefined"
        c = env[payload(getop(ir, s, 1))]::CellBox
        env[s.id] = c.defined
    elseif k === K"throw_undef_if_not"
        cond = opval(ir, env, getop(ir, s, 1))::Bool
        cond || throw(UndefVarError(opval(ir, env, getop(ir, s, 2))::Symbol))
    elseif k === K"globalref"
        env[s.id] = opval(ir, env, getop(ir, s, 1))
    elseif k === K"boundscheck"
        env[s.id] = true
    elseif k === K"gc_preserve_begin"
        env[s.id] = nothing
    elseif k === K"gc_preserve_end" || k === K"latestworld" || k === K"coverage_effect"
        # no-op
    elseif k === K"new"
        t = opval(ir, env, getop(ir, s, 1))
        fields = vals(ir, env, s, 2)
        env[s.id] = ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32),
                          t, fields, length(fields))
    else
        error("interpret: no semantics for kind $(q)")
    end
    return nothing
end
