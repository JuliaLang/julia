# Exception-handler entry conversion: CodeInfo with EnterNode/:leave/
# :pop_exception → UnifiedIR `try` regions with nested cfg islands.
#
# Scope model (from the same analysis stock inference uses,
# Compiler.compute_trycatch): every handler h yields a body scope (statements
# protected by h) and a catch scope (statements running under h's exception).
# Each scope becomes one cfg island; handler h becomes
#     try { cfg{…} unreachable } catch (%exc) { cfg{…} unreachable }
# placed in its parent's island. Control never falls out of a `try`: every
# normal exit of a body/handler island is a sealed cross-island `goto` to an
# ancestor island's block (§5.5/§5.9); the exit converter re-synthesizes the
# :leave/:pop_exception actions from the region structure. SSA values used
# outside their defining scope are demoted to frame cells — §6's rule that
# values live across the try boundary keep memory form.

struct EHScope
    id::Int              # 0 = root; 1..H body scopes; H+1..2H catch scopes
    handler::Int         # handler index (0 for root)
end

function codeinfo_to_ir_eh(ci::Core.CodeInfo; nargs::Int, name::Symbol)
    code = ci.code
    n = length(code)
    nslots = length(ci.slotnames)
    hinfo = Compiler.compute_trycatch(code)
    handlers = hinfo.handlers
    H = length(handlers)
    handler_at = hinfo.handler_at

    enter_of = zeros(Int, H)
    catchdest_of = zeros(Int, H)
    for h in 1:H
        e = handlers[h].enter_idx
        enter_of[h] = e
        en = code[e]::Core.EnterNode
        en.catch_dest == 0 &&
            throw(UnsupportedIR("EnterNode with no catch destination (scoped-value enter)"))
        catchdest_of[h] = en.catch_dest
    end

    # ---- scope of each statement -------------------------------------------
    # node ids: 0 root, h body, H+h catch
    node_memo = fill(-1, n)
    function node_of(i::Int)::Int
        node_memo[i] >= 0 && return node_memo[i]
        hb, he = handler_at[i]
        r = if hb == 0 && he == 0
            0
        elseif he == 0
            Int(hb)
        elseif hb == 0
            H + Int(he)
        else
            # deeper of body(hb) / catch(he): body(hb) is deeper iff catch(he)
            # is an ancestor of body(hb), i.e. reachable from enter(hb)'s node
            a = node_of(enter_of[hb])
            deeper_body = false
            while true
                if a == H + Int(he)
                    deeper_body = true
                    break
                end
                a == 0 && break
                a = parent_node(a)
            end
            deeper_body ? Int(hb) : H + Int(he)
        end
        node_memo[i] = r
        return r
    end
    parent_node(node::Int) = node == 0 ? 0 :
        node_of(enter_of[node <= H ? node : node - H])

    # ---- micro-blocks --------------------------------------------------------
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
        elseif st isa Core.EnterNode
            isleader[st.catch_dest] = true
            i < n && (isleader[i + 1] = true)
        elseif st isa Core.PhiNode || st isa Core.PhiCNode || st isa Core.UpsilonNode
            throw(UnsupportedIR("$(typeof(st)) in slot-form input"))
        end
    end
    for i in 2:n
        node_of(i) != node_of(i - 1) && (isleader[i] = true)
    end
    leaders = [i for i in 1:n if isleader[i]]
    nblocks = length(leaders)
    blockof = zeros(Int, n)
    for (bi, l) in enumerate(leaders)
        hi = bi < nblocks ? leaders[bi + 1] - 1 : n
        for i in l:hi
            blockof[i] = bi
        end
    end
    node_blocks = Dict{Int,Vector{Int}}()
    for bi in 1:nblocks
        push!(get!(() -> Int[], node_blocks, node_of(leaders[bi])), bi)
    end

    # ---- cross-scope SSA uses → demoted to cells ----------------------------
    demote = falses(n)
    function scan_ssa(i::Int, @nospecialize(v))
        if v isa Core.SSAValue
            node_of(v.id) != node_of(i) && (demote[v.id] = true)
        elseif v isa Expr
            for a in v.args
                scan_ssa(i, a)
            end
        elseif v isa Core.GotoIfNot
            scan_ssa(i, v.cond)
        elseif v isa Core.ReturnNode
            isdefined(v, :val) && scan_ssa(i, v.val)
        end
    end
    for (i, st) in enumerate(code)
        (st isa Expr && (st.head === :leave || st.head === :pop_exception)) && continue
        scan_ssa(i, st)
    end
    # gc_preserve tokens are pairing links, not values: keep direct references
    for (i, st) in enumerate(code)
        if st isa Expr && st.head === :gc_preserve_begin
            demote[i] = false
        end
    end

    # ---- builder -------------------------------------------------------------
    b = UnifiedIR.Builder(; name)
    argmap = Vector{StmtId}(undef, nargs)
    for i in 1:nargs
        argmap[i] = append_stmt!(b, K"region_arg"; type = Any)
        push!(b.ir.argtypes, Any)
    end
    cellmap = Dict{Int,StmtId}()          # slot -> cell
    for sl in (nargs+1):nslots
        cellmap[sl] = append_stmt!(b, K"cell", Any; type = Any)
    end
    democell = Dict{Int,StmtId}()         # demoted SSA idx -> cell
    for i in 1:n
        demote[i] && (democell[i] = append_stmt!(b, K"cell", Any; type = Any))
    end

    # pre-create every micro-block region (owners fixed at scope emission)
    blockregions = Vector{RegionId}(undef, nblocks)
    for bi in 1:nblocks
        r = UnifiedIR.Region(UnifiedIR.REGION_BLOCK, NULL_STMT, NULL_REGION)
        push!(b.ir.regions, r)
        blockregions[bi] = RegionId(length(b.ir.regions))
    end

    ssamap = Vector{Any}(undef, n)
    excarg = Dict{Int,StmtId}()           # handler idx -> %exc region_arg
    curnode = Ref(0)

    function convert_value(@nospecialize(v))::UnifiedIR.Operand
        if v isa Core.SSAValue
            if demote[v.id] && node_of(v.id) != curnode[]
                g = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(democell[v.id]); type = Any)
                return UnifiedIR.op_stmt(g)
            end
            o = ssamap[v.id]
            o isa UnifiedIR.Operand || throw(UnsupportedIR("forward SSA reference"))
            return o
        elseif v isa Core.SlotNumber
            v.id <= nargs && return UnifiedIR.op_stmt(argmap[v.id])
            g = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(cellmap[v.id]); type = Any)
            return UnifiedIR.op_stmt(g)
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

    function record!(i::Int, o::UnifiedIR.Operand)
        ssamap[i] = o
        if demote[i]
            append_stmt!(b, K"cell_set", UnifiedIR.op_stmt(democell[i]), o)
        end
        return nothing
    end

    goto_block!(bi::Int) =
        append_stmt!(b, K"goto", UnifiedIR.op_block(blockregions[bi]), UnifiedIR.op_inline(0))

    # emit one scope: a cfg op whose blocks are the scope's micro-blocks
    function emit_scope!(node::Int)
        bs = get(node_blocks, node, Int[])
        cfgop = append_stmt!(b, K"cfg"; type = Any)
        for bi in bs
            rid = blockregions[bi]
            reg = UnifiedIR.getregion(b.ir, rid)
            reg.owner = cfgop
            reg.parent = UnifiedIR.current_region(b)
            reg.first = StmtId(Int(b.ir.body.len) + 1)
            push!(b.open, rid)
            emit_block!(bi, node)
            reg.last = StmtId(Int(b.ir.body.len))
            pop!(b.open)
        end
        return cfgop
    end

    function emit_block!(bi::Int, node::Int)
        curnode[] = node
        lo = leaders[bi]
        hi = bi < nblocks ? leaders[bi + 1] - 1 : n
        terminated = false
        i = lo
        while i <= hi
            st = code[i]
            if st isa Core.EnterNode
                h = 0
                for hh in 1:H
                    enter_of[hh] == i && (h = hh)
                end
                h == 0 && throw(UnsupportedIR("unregistered EnterNode"))
                emit_try!(h, node)
                curnode[] = node
                append_stmt!(b, K"unreachable")
                terminated = true
                break
            elseif st isa Core.GotoNode
                goto_block!(blockof[st.label])
                terminated = true
            elseif st isa Core.GotoIfNot
                cond = convert_value(st.cond)
                fallbi = blockof[i + 1]
                append_stmt!(b, K"br_if", cond,
                             UnifiedIR.op_block(blockregions[fallbi]), UnifiedIR.op_inline(0),
                             UnifiedIR.op_block(blockregions[blockof[st.dest]]), UnifiedIR.op_inline(0))
                terminated = true
            elseif st isa Core.ReturnNode
                if isdefined(st, :val)
                    append_stmt!(b, K"return", convert_value(st.val))
                else
                    append_stmt!(b, K"unreachable")
                end
                terminated = true
            elseif st isa Core.NewvarNode
                sl = st.slot.id
                haskey(cellmap, sl) && append_stmt!(b, K"cell_new", UnifiedIR.op_stmt(cellmap[sl]))
                ssamap[i] = UnifiedIR.vop(b.ir, nothing)
            elseif st isa Expr && st.head === :leave
                ssamap[i] = UnifiedIR.vop(b.ir, nothing)      # structural (§5.9)
            elseif st isa Expr && st.head === :pop_exception
                ssamap[i] = UnifiedIR.vop(b.ir, nothing)      # structural
            elseif st isa Expr && st.head === :the_exception
                he = Int(handler_at[i][2])
                he != 0 || throw(UnsupportedIR(":the_exception outside a catch scope"))
                record!(i, UnifiedIR.op_stmt(excarg[he]))
            elseif st isa Expr
                emit_expr!(i, st)
            else
                # bare value statement
                record!(i, convert_value(st))
            end
            i += 1
        end
        if !terminated
            # implicit fallthrough → explicit (possibly cross-island) goto
            hi >= n && throw(UnsupportedIR("function falls off the end"))
            goto_block!(blockof[hi + 1])
        end
        return nothing
    end

    function emit_try!(h::Int, parentnode::Int)
        en = code[enter_of[h]]::Core.EnterNode
        ops = UnifiedIR.Operand[]
        if isdefined(en, :scope)
            push!(ops, convert_value(en.scope))
        end
        tryop = append_stmt!(b, K"try", ops...; type = Any)
        ssamap[enter_of[h]] = UnifiedIR.vop(b.ir, nothing)   # token: structural
        # body region: nested island of the body scope
        UnifiedIR.open_region!(b, tryop; kind = UnifiedIR.REGION_BODY)
        emit_scope!(h)
        append_stmt!(b, K"unreachable")
        UnifiedIR.close_region!(b)
        # handler region: %exc arg + nested island of the catch scope
        UnifiedIR.open_region!(b, tryop; kind = UnifiedIR.REGION_HANDLER)
        exc = append_stmt!(b, K"region_arg"; type = Any)
        excarg[h] = exc
        emit_scope!(H + h)
        append_stmt!(b, K"unreachable")
        UnifiedIR.close_region!(b)
        return tryop
    end

    function emit_expr!(i::Int, st::Expr)
        h = st.head
        if h === :(=)
            lhs = st.args[1]
            rhs = st.args[2]
            rhsop = if rhs isa Expr
                emit_expr!(i, rhs)
                ssamap[i]
            else
                convert_value(rhs)
            end
            lhs isa Core.SlotNumber || throw(UnsupportedIR("assignment to $(typeof(lhs))"))
            lhs.id <= nargs && throw(UnsupportedIR("assignment to argument slot"))
            append_stmt!(b, K"cell_set", UnifiedIR.op_stmt(cellmap[lhs.id]), rhsop)
            ssamap[i] = rhsop
        elseif h === :call
            record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"call",
                UnifiedIR.Operand[convert_value(a) for a in st.args]...; type = Any)))
        elseif h === :invoke
            tgt = st.args[1]
            tgt isa Union{Core.MethodInstance,Core.CodeInstance} ||
                throw(UnsupportedIR("invoke with non-instance target"))
            ops = UnifiedIR.Operand[UnifiedIR.vop(b.ir, tgt)]
            for a in st.args[2:end]
                push!(ops, convert_value(a))
            end
            record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"invoke", ops...; type = Any)))
        elseif h === :new
            record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"new",
                UnifiedIR.Operand[convert_value(a) for a in st.args]...; type = Any)))
        elseif h === :splatnew
            record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"splatnew",
                UnifiedIR.Operand[convert_value(a) for a in st.args]...; type = Any)))
        elseif h === :isdefined
            a = st.args[1]
            if a isa Core.SlotNumber && a.id > nargs
                record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"cell_isdefined",
                    UnifiedIR.op_stmt(cellmap[a.id]); type = Bool)))
            elseif a isa GlobalRef
                record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"isdefined_global",
                    UnifiedIR.vop(b.ir, a); type = Bool)))
            else
                ssamap[i] = UnifiedIR.op_inline(true)
            end
        elseif h === :throw_undef_if_not
            name_, cond = st.args
            append_stmt!(b, K"throw_undef_if_not", convert_value(cond),
                         UnifiedIR.vop(b.ir, name_ isa Symbol ? name_ : Symbol(name_)))
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :boundscheck
            record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"boundscheck"; type = Bool)))
        elseif h === :static_parameter
            ssamap[i] = UnifiedIR.op_sparam(st.args[1]::Int)
        elseif h === :meta || h === :inbounds || h === :loopinfo || h === :aliasscope ||
               h === :popaliasscope || h === :inline || h === :noinline || h === :purity
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :code_coverage_effect
            append_stmt!(b, K"coverage_effect")
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :gc_preserve_begin
            record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"gc_preserve_begin",
                UnifiedIR.Operand[convert_value(a) for a in st.args]...; type = Any)))
        elseif h === :gc_preserve_end
            append_stmt!(b, K"gc_preserve_end", convert_value(st.args[1]))
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :latestworld
            append_stmt!(b, K"latestworld")
            ssamap[i] = UnifiedIR.vop(b.ir, nothing)
        elseif h === :foreigncall || h === :cfunction
            ops = UnifiedIR.Operand[]
            for a in st.args
                push!(ops, a isa Union{Core.SSAValue,Core.SlotNumber,Core.Argument} ?
                      convert_value(a) : UnifiedIR.vop(b.ir, a))
            end
            record!(i, UnifiedIR.op_stmt(append_stmt!(b,
                h === :foreigncall ? K"foreigncall" : K"cfunction", ops...; type = Any)))
        elseif h === :copyast
            record!(i, UnifiedIR.op_stmt(append_stmt!(b, K"copyast",
                convert_value(st.args[1]); type = Any)))
        elseif h === :the_exception
            he = Int(handler_at[i][2])
            he != 0 || throw(UnsupportedIR(":the_exception outside a catch scope"))
            record!(i, UnifiedIR.op_stmt(excarg[he]))
        elseif h === :method
            throw(UnsupportedIR("nested :method definition"))
        elseif h === :globaldecl || h === :const
            throw(UnsupportedIR("toplevel form :$h"))
        else
            throw(UnsupportedIR("Expr head :$h"))
        end
        return nothing
    end

    root_cfg = emit_scope!(0)
    append_stmt!(b, K"return", UnifiedIR.op_stmt(root_cfg))
    ir = UnifiedIR.finish!(b; verify = false)
    UnifiedIR.verify_ir(ir; level = 0)
    return ir
end
