function ssaargmap(f, @nospecialize(stmt))
    urs = userefs(stmt)
    urs === () && return stmt
    for op in urs
        val = op[]
        if isa(val, Union{SSAValue, Argument})
            op[] = f(val)
        end
    end
    urs[]
end

function replace_code!(ci::CodeInfo, code::IRCode, nargs::Int, topline::LineNumberNode)
    if !isempty(code.new_nodes)
        code = compact!(code)
    end
    # All but the first `nargs` slots will now be unused
    resize!(ci.slottypes, nargs+1)
    resize!(ci.slotnames, nargs+1)
    resize!(ci.slotflags, nargs+1)
    # For every used SSAValues, we register one base format ssa value
    used = IdSet{Int}()
    foreach(stmt->scan_ssa_use!(used, stmt), code.stmts)
    mapping = IdDict{Int, Int}()
    n = 0
    resize!(ci.ssavaluetypes, length(used))
    for ssa in sort(Int[x for x in used])
        mapping[ssa] = n
        n += 1
        ci.ssavaluetypes[n] = code.types[ssa]
    end
    # Find all jump targets (we need to insert LabelNodes for them) and
    # jump origins (we insert a label node on the statement after, to
    # make sure we can track them)
    dest_blocks = IdSet{Int}()
    jump_origins = IdSet{Int}()
    for stmt in code.stmts
        if isa(stmt, GotoNode)
            push!(dest_blocks, stmt.label)
        elseif isa(stmt, GotoIfNot)
            push!(dest_blocks, stmt.dest)
        elseif isa(stmt, PhiNode)
            for edge in stmt.edges
                push!(jump_origins, edge)
            end
        end
    end
    cfg = code.cfg
    block_start = IdDict{Int, Int}(first(cfg.blocks[x].stmts)=>x for x in dest_blocks)
    comefrom_labels = IdSet{Int}(last(cfg.blocks[x].stmts)+1 for x in jump_origins)
    block_terminators = IdDict{Int, Int}(last(block.stmts)=>i for (i,block) in pairs(cfg.blocks))
    local rename
    let mapping = mapping
        function rename(@nospecialize(val))
            if isa(val, SSAValue)
                if haskey(mapping, val.id)
                    return SSAValue(mapping[val.id])
                end
            elseif isa(val, Argument)
                return SlotNumber(val.n)
            end
            return val
        end
    end
    # Now translate the code
    new_code = Vector{Any}()
    append!(new_code, code.meta)
    label_mapping = IdDict{Int, Int}()
    terminator_mapping = IdDict{Int, Int}()
    fixup = Int[]
    for (idx, stmt) in pairs(code.stmts)
        line = code.lines[idx]
        # push labels first
        if haskey(block_start, idx)
            push!(new_code, LabelNode(length(new_code) + 1))
            label_mapping[block_start[idx]] = length(new_code)
        elseif idx in comefrom_labels
            push!(new_code, LabelNode(length(new_code) + 1))
        end
        # then metadata
        if !(line.file === nothing && line.line === 0) && !(line === topline)
            push!(new_code, line)
            topline = line
        end
        # record if this'll need a fixup after stmt number
        if isa(stmt, GotoIfNot)
            new_stmt = Expr(:gotoifnot, rename(stmt.cond), stmt.dest)
            push!(fixup, length(new_code)+1)
        elseif isa(stmt, ReturnNode)
            if isdefined(stmt, :val)
                new_stmt = Expr(:return, rename(stmt.val))
            else
                # Unreachable, so no issue with this
                new_stmt = nothing
            end
        elseif isa(stmt, SSAValue)
            new_stmt = rename(stmt)
        elseif isa(stmt, PhiNode)
            new_stmt = ssaargmap(rename, stmt)
            push!(fixup, length(new_code)+1)
        elseif isa(stmt, GotoNode)
            push!(fixup, length(new_code)+1)
            new_stmt = stmt
        else
            new_stmt = ssaargmap(rename, stmt)
        end
        if haskey(mapping, idx)
            new_stmt = Expr(:(=), SSAValue(mapping[idx]), new_stmt)
        end
        # record fixup targets
        if haskey(block_terminators, idx)
            terminator_mapping[block_terminators[idx]] = length(new_code)+1
        end
        # and finally, record the new new statement
        push!(new_code, new_stmt)
    end
    for i in fixup
        val = new_code[i]
        isassign = isexpr(val, :(=))
        if isassign
            val = val.args[2]
        end
        if isa(val, PhiNode)
            # Translate from BB edges to statement edges
            edges = Any[terminator_mapping[edge] for edge in val.edges]
            val = PhiNode(convert(Vector{Any}, edges), val.values)
        elseif isa(val, GotoNode)
            val = GotoNode(label_mapping[val.label])
        elseif isexpr(val, :gotoifnot)
            val = Expr(:gotoifnot, val.args[1], label_mapping[val.args[2]])
        else
            #@show val
            error()
        end
        if isassign
            new_code[i].args[2] = val
        else
            new_code[i] = val
        end
    end
    ci.code = new_code
    return ci
end
