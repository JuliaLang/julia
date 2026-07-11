# Entry converter, CodeInfo → UnifiedIR (§10.5), cfg-wrap mode: the whole
# body becomes one `cfg` island; slots become cells. Always available for the
# supported feature matrix; structurization is a separate mode (P1).
#
# Feature matrix (v1): no exception handlers (EnterNode/:leave/:pop_exception),
# no PhiNode/PhiCNode/UpsilonNode (uninferred slot-form code has none).

struct UnsupportedIR <: Exception
    what::String
end
Base.showerror(io::IO, e::UnsupportedIR) = print(io, "UnsupportedIR: ", e.what)

"First-operand marker distinguishing an `Expr(:foreignglobal, name)` (the
cglobal lowering, rt Ptr{Cvoid}) encoded on the K\"foreigncall\" kind."
const FOREIGNGLOBAL_MARKER = Symbol("unified.foreignglobal")

"""
    codeinfo_to_ir(ci::Core.CodeInfo; nargs, name=:f) -> UnifiedIR.IR

cfg-wrap conversion of slot-form lowered code. `nargs` counts the function
slots including slot 1 (`#self#`).
"""
function codeinfo_to_ir(ci::Core.CodeInfo; nargs::Int, name::Symbol = :f)
    code = ci.code
    # exception handlers take the scope-recovering path (eh_entry.jl)
    if any(st -> st isa Core.EnterNode || Meta.isexpr(st, :enter) ||
                 Meta.isexpr(st, :leave) || Meta.isexpr(st, :pop_exception) ||
                 Meta.isexpr(st, :the_exception), code)
        return codeinfo_to_ir_eh(ci; nargs, name)
    end
    n = length(code)
    nslots = length(ci.slotnames)

    # -- block structure ----------------------------------------------------
    isleader = falses(n + 1)
    isleader[1] = true
    for (i, st) in enumerate(code)
        if st isa Core.GotoNode
            isleader[st.label] = true
            i < n && (isleader[i + 1] = true)
        elseif st isa Core.GotoIfNot
            isleader[st.dest] = true
            i < n && (isleader[i + 1] = true)
        elseif st isa Core.ReturnNode
            i < n && (isleader[i + 1] = true)
        elseif st isa Core.EnterNode || Meta.isexpr(st, :enter) ||
               Meta.isexpr(st, :leave) || Meta.isexpr(st, :pop_exception)
            throw(UnsupportedIR("exception handler IR (EnterNode/:leave) — outside the cfg-wrap v1 feature matrix"))
        elseif st isa Core.PhiNode || st isa Core.PhiCNode || st isa Core.UpsilonNode
            throw(UnsupportedIR("$(typeof(st)) in slot-form input"))
        end
    end
    leaders = [i for i in 1:n if isleader[i]]
    blockof = zeros(Int, n)                 # stmt -> block index
    for (bi, l) in enumerate(leaders)
        hi = bi < length(leaders) ? leaders[bi + 1] - 1 : n
        for i in l:hi
            blockof[i] = bi
        end
    end
    nblocks = length(leaders)

    # -- builder ------------------------------------------------------------
    b = UnifiedIR.Builder(; name)
    argmap = Vector{StmtId}(undef, nargs)
    for i in 1:nargs
        t = ci.slottypes === nothing ? Any : something(ci.slottypes[i], Any)
        argmap[i] = append_stmt!(b, K"region_arg"; type = t isa Type ? t : Any)
        push!(b.ir.argtypes, Any)
    end
    # cells for non-argument slots
    cellmap = Dict{Int,StmtId}()
    for sl in (nargs+1):nslots
        cellmap[sl] = append_stmt!(b, K"cell", Any; type = Any)
    end

    single = nblocks == 1 && !any(st -> st isa Core.GotoNode || st isa Core.GotoIfNot, code)

    cfgop = NULL_STMT
    blockregions = RegionId[]
    if !single
        cfgop = append_stmt!(b, K"cfg"; type = Any)
    end

    ssamap = Vector{Any}(undef, n)          # CodeInfo ssa idx -> Operand
    debugtriple(i) = (Int32(0), Int32(0), Int32(0))

    function convert_value(@nospecialize(v))::UnifiedIR.Operand
        if v isa Core.SSAValue
            o = ssamap[v.id]
            o isa UnifiedIR.Operand || throw(UnsupportedIR("forward SSA reference"))
            return o
        elseif v isa Core.SlotNumber
            if v.id <= nargs
                return UnifiedIR.op_stmt(argmap[v.id])
            else
                g = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(cellmap[v.id]); type = Any)
                return UnifiedIR.op_stmt(g)
            end
        elseif v isa Core.Argument
            return UnifiedIR.op_stmt(argmap[v.n])
        elseif v isa GlobalRef
            return UnifiedIR.vop(b.ir, v)
        elseif v isa QuoteNode
            return UnifiedIR.vop(b.ir, v.value)
        elseif v isa Expr
            v.head === :static_parameter && return UnifiedIR.op_sparam(v.args[1]::Int)
            throw(UnsupportedIR("nested Expr operand $(v.head)"))
        else
            return UnifiedIR.vop(b.ir, v)
        end
    end

    returns = 0
    function convert_stmt!(i::Int, st)
        if st isa Core.ReturnNode
            isdefined(st, :val) || begin
                append_stmt!(b, K"unreachable")
                return
            end
            v = convert_value(st.val)
            if single
                append_stmt!(b, K"return", v)
            else
                append_stmt!(b, K"yield", v)
            end
            returns += 1
        elseif st isa Core.GotoNode
            dest = blockregions[blockof[st.label]]
            append_stmt!(b, K"goto", UnifiedIR.op_block(dest), UnifiedIR.op_inline(0))
        elseif st isa Core.GotoIfNot
            cond = convert_value(st.cond)
            fall = blockregions[blockof[i] + 1]
            dest = blockregions[blockof[st.dest]]
            append_stmt!(b, K"br_if", cond,
                         UnifiedIR.op_block(fall), UnifiedIR.op_inline(0),
                         UnifiedIR.op_block(dest), UnifiedIR.op_inline(0))
        elseif st isa Core.NewvarNode
            sl = st.slot.id
            haskey(cellmap, sl) && append_stmt!(b, K"cell_new", UnifiedIR.op_stmt(cellmap[sl]))
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif st isa Expr
            convert_expr!(i, st)
        elseif st isa Core.SlotNumber || st isa Core.SSAValue || st isa GlobalRef ||
               st isa QuoteNode || !(st isa Union{Core.GotoNode,Core.GotoIfNot})
            # bare value statement: its SSA value is the value itself
            ssamap[i] = convert_value(st)
        end
        return
    end

    function convert_expr!(i::Int, st::Expr)
        h = st.head
        if h === :(=)
            lhs = st.args[1]
            rhs = st.args[2]
            rhsop = if rhs isa Expr
                convert_expr!(i, rhs)
                ssamap[i]
            else
                convert_value(rhs)
            end
            lhs isa Core.SlotNumber || throw(UnsupportedIR("assignment to $(typeof(lhs))"))
            if lhs.id <= nargs
                throw(UnsupportedIR("assignment to argument slot"))
            end
            append_stmt!(b, K"cell_set", UnifiedIR.op_stmt(cellmap[lhs.id]), rhsop)
            ssamap[i] = rhsop
        elseif h === :call
            ops = UnifiedIR.Operand[convert_value(a) for a in st.args]
            s = append_stmt!(b, K"call", ops...; type = Any)
            ssamap[i] = UnifiedIR.op_stmt(s)
        elseif h === :invoke
            tgt = st.args[1]
            tgt isa Union{Core.MethodInstance,Core.CodeInstance} ||
                throw(UnsupportedIR("invoke with non-instance target"))
            ops = UnifiedIR.Operand[UnifiedIR.vop(b.ir, tgt)]
            for a in st.args[2:end]
                push!(ops, convert_value(a))
            end
            s = append_stmt!(b, K"invoke", ops...; type = Any)
            ssamap[i] = UnifiedIR.op_stmt(s)
        elseif h === :new
            ops = UnifiedIR.Operand[convert_value(a) for a in st.args]
            s = append_stmt!(b, K"new", ops...; type = Any)
            ssamap[i] = UnifiedIR.op_stmt(s)
        elseif h === :splatnew
            ops = UnifiedIR.Operand[convert_value(a) for a in st.args]
            s = append_stmt!(b, K"splatnew", ops...; type = Any)
            ssamap[i] = UnifiedIR.op_stmt(s)
        elseif h === :isdefined
            a = st.args[1]
            if a isa Core.SlotNumber && a.id > nargs
                s = append_stmt!(b, K"cell_isdefined", UnifiedIR.op_stmt(cellmap[a.id]); type = Bool)
                ssamap[i] = UnifiedIR.op_stmt(s)
            elseif a isa GlobalRef
                s = append_stmt!(b, K"isdefined_global", UnifiedIR.vop(b.ir, a); type = Bool)
                ssamap[i] = UnifiedIR.op_stmt(s)
            else
                ssamap[i] = UnifiedIR.op_inline(true)
            end
        elseif h === :throw_undef_if_not
            name_, cond = st.args
            append_stmt!(b, K"throw_undef_if_not", convert_value(cond),
                         UnifiedIR.vop(b.ir, name_ isa Symbol ? name_ : Symbol(name_)))
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :boundscheck
            s = append_stmt!(b, K"boundscheck"; type = Bool)
            ssamap[i] = UnifiedIR.op_stmt(s)
        elseif h === :static_parameter
            ssamap[i] = UnifiedIR.op_sparam(st.args[1]::Int)
        elseif h === :meta || h === :inbounds || h === :loopinfo || h === :aliasscope ||
               h === :popaliasscope || h === :inline || h === :noinline || h === :purity
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)  # carried as flags/columns later
        elseif h === :code_coverage_effect
            append_stmt!(b, K"coverage_effect")
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :gc_preserve_begin
            ops = UnifiedIR.Operand[convert_value(a) for a in st.args]
            s = append_stmt!(b, K"gc_preserve_begin", ops...; type = Any)
            ssamap[i] = UnifiedIR.op_stmt(s)
        elseif h === :gc_preserve_end
            append_stmt!(b, K"gc_preserve_end", convert_value(st.args[1]))
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :latestworld
            append_stmt!(b, K"latestworld")
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :foreigncall || h === :cfunction || h === :foreignglobal
            # operands: all pieces; non-value pieces interned as constants.
            # :foreignglobal (the cglobal lowering; rt Ptr{Cvoid}) rides the
            # foreigncall kind behind a marker first operand — the transfer
            # and the exit converter both recognize FOREIGNGLOBAL_MARKER.
            ops = UnifiedIR.Operand[]
            h === :foreignglobal &&
                push!(ops, UnifiedIR.vop(b.ir, FOREIGNGLOBAL_MARKER))
            for a in st.args
                push!(ops, a isa Union{Core.SSAValue,Core.SlotNumber,Core.Argument} ?
                      convert_value(a) : UnifiedIR.vop(b.ir, a))
            end
            s = append_stmt!(b, h === :cfunction ? K"cfunction" : K"foreigncall",
                             ops...; type = Any)
            ssamap[i] = UnifiedIR.op_stmt(s)
        elseif h === :the_exception || h === :enter || h === :leave || h === :pop_exception
            throw(UnsupportedIR("exception IR ($h) — outside the v1 feature matrix"))
        elseif h === :method
            throw(UnsupportedIR("nested :method definition"))
        elseif h === :copyast
            s = append_stmt!(b, K"copyast", convert_value(st.args[1]); type = Any)
            ssamap[i] = UnifiedIR.op_stmt(s)
        elseif h === :globaldecl || h === :const
            throw(UnsupportedIR("toplevel form :$h"))
        else
            throw(UnsupportedIR("Expr head :$h"))
        end
        return
    end

    if single
        for (i, st) in enumerate(code)
            convert_stmt!(i, st)
        end
    else
        # pre-create block regions so edges can reference them
        for bi in 1:nblocks
            r = UnifiedIR.Region(UnifiedIR.REGION_BLOCK, cfgop, UnifiedIR.stmt_region(b.ir, cfgop))
            push!(b.ir.regions, r)
            push!(blockregions, RegionId(length(b.ir.regions)))
        end
        for bi in 1:nblocks
            rid = blockregions[bi]
            reg = UnifiedIR.getregion(b.ir, rid)
            reg.first = StmtId(Int(b.ir.body.len) + 1)
            push!(b.open, rid)
            lo = leaders[bi]
            hi = bi < nblocks ? leaders[bi + 1] - 1 : n
            for i in lo:hi
                convert_stmt!(i, code[i])
            end
            # implicit fallthrough becomes explicit goto (§5.5)
            lastst = code[hi]
            if !(lastst isa Core.GotoNode || lastst isa Core.GotoIfNot ||
                 lastst isa Core.ReturnNode)
                bi == nblocks && throw(UnsupportedIR("function falls off the end"))
                append_stmt!(b, K"goto", UnifiedIR.op_block(blockregions[bi + 1]),
                             UnifiedIR.op_inline(0))
            end
            reg.last = StmtId(Int(b.ir.body.len))
            pop!(b.open)
        end
        r = append_stmt!(b, K"return", UnifiedIR.op_stmt(cfgop))
    end

    ir = UnifiedIR.finish!(b; verify = false)
    UnifiedIR.verify_ir(ir; level = 0)
    return ir
end
