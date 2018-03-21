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

function line_to_vector(line::Int, linetable::Vector{LineInfoNode})
    lines = Int[]
    while line != 0
        push!(lines, line)
        line = linetable[line].inlined_at
    end
    return lines
end

function push_new_lineinfo!(new_code::Vector{Any}, topline::Int, line::Int, linetable::Vector{LineInfoNode})
    # separate the info into three sets: pops, line-change, pushes
    do_coverage = coverage_enabled()
    topmod = linetable[line].mod
    toplines = line_to_vector(topline, linetable)
    lines = line_to_vector(line, linetable)
    while !isempty(lines) && !isempty(toplines) && lines[end] == toplines[end]
        # remove common frames, recording changes to topmod
        topmod = linetable[pop!(lines)].mod
        pop!(toplines)
    end
    # check whether the outermost frame changed, or just the line number
    newframe = true
    topfile = NullLineInfo.file
    if !isempty(lines) && !isempty(toplines)
        let topline = linetable[toplines[end]]
            line = linetable[lines[end]]
            if topline.inlined_at == 0 || (topline.mod === line.mod && topline.method === line.method)
                # we could track frame_id precisely, but llvm / dwarf has no support for that,
                # and it wouldn't really be that meaningful after statements moved around,
                # so we just do fuzzy matching here in the legacy-format writer
                newframe = false
                topfile = topline.file
            end
        end
    end
    # first pop the old frame(s)
    npops = length(toplines) + newframe - 1
    if npops > 0
        push!(new_code, (npops == 1) ? Expr(:meta, :pop_loc) : Expr(:meta, :pop_loc, npops))
    end
    # then change the line number
    if !newframe
        let line = linetable[pop!(lines)]
            if line.file === topfile
                loc = LineNumberNode(line.line)
            else
                loc = LineNumberNode(line.line, line.file)
            end
            push!(new_code, loc)
            topmod = line.mod
        end
    end
    # then push the new frames
    while !isempty(lines)
        let line = linetable[pop!(lines)]
            if !do_coverage || line.mod == topmod
                loc = Expr(:meta, :push_loc, line.file, line.method, line.line)
            else
                loc = Expr(:meta, :push_loc, line.file, line.method, line.line, line.mod)
            end
            push!(new_code, loc)
            topmod = line.mod
        end
    end
    nothing
end

function replace_code!(ci::CodeInfo, code::IRCode, nargs::Int, linetable::Vector{LineInfoNode})
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
    topline = 1
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
        if line != 0 && line != topline
            push_new_lineinfo!(new_code, topline, line, linetable)
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
                new_stmt = Expr(:unreachable)
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
            if isa(new_stmt, Expr)
                new_stmt.typ = code.types[idx]
            end
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
