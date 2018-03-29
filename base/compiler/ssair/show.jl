function Base.show(io::IO, cfg::CFG)
    foreach(pairs(cfg.blocks)) do (idx, block)
        println("$idx\t=>\t", join(block.succs, ", "))
    end
end

print_ssa(io::IO, val) = isa(val, SSAValue) ? print(io, "%$(val.id)") : print(io, val)
function print_node(io::IO, idx, stmt, used, maxsize; color = true, print_typ=true)
    if idx in used
        pad = " "^(maxsize-length(string(idx)))
        print(io, "%$idx $pad= ")
    else
        print(io, " "^(maxsize+4))
    end
    if isa(stmt, PhiNode)
        args = map(1:length(stmt.edges)) do i
            e = stmt.edges[i]
            v = !isassigned(stmt.values, i) ? "#undef" :
                sprint() do io′
                    print_ssa(io′, stmt.values[i])
                end
            "$e => $v"
        end
        print(io, "φ ", '(', join(args, ", "), ')')
    elseif isa(stmt, PiNode)
        print(io, "π (")
        print_ssa(io, stmt.val)
        print(io, ", ")
        if color
            printstyled(io, stmt.typ, color=:red)
        else
            print(io, stmt.typ)
        end
        print(io, ")")
    elseif isa(stmt, ReturnNode)
        if !isdefined(stmt, :val)
            print(io, "unreachable")
        else
            print(io, "return ")
            print_ssa(io, stmt.val)
        end
    elseif isa(stmt, GotoIfNot)
        print(io, "goto ", stmt.dest, " if not ")
        print_ssa(io, stmt.cond)
    elseif isexpr(stmt, :call)
        print_ssa(io, stmt.args[1])
        print(io, "(")
        print(io, join(map(arg->sprint(io->print_ssa(io, arg)), stmt.args[2:end]), ", "))
        print(io, ")")
        if print_typ && stmt.typ !== Any
            print(io, "::$(stmt.typ)")
        end
    elseif isexpr(stmt, :new)
        print(io, "new(")
        print(io, join(map(arg->sprint(io->print_ssa(io, arg)), stmt.args), ", "))
        print(io, ")")
    else
        print(io, stmt)
    end
end

function Base.show(io::IO, code::IRCode)
    io = IOContext(io, :color=>true)
    used = Set{Int}()
    println(io, "Code")
    foreach(stmt->scan_ssa_use!(used, stmt), code.stmts)
    foreach(((_a, _b, node, _d),) -> scan_ssa_use!(used, node), code.new_nodes)
    if isempty(used)
        maxsize = 0
    else
        maxused = maximum(used)
        maxsize = length(string(maxused))
    end
    cfg = code.cfg
    max_bb_idx_size = length(string(length(cfg.blocks)))
    bb_idx = 1
    perm = sortperm(code.new_nodes, by = x->x[1])
    new_nodes_perm = Iterators.Stateful(perm)
    for (idx, stmt) in Iterators.enumerate(code.stmts)
        bbrange = cfg.blocks[bb_idx].stmts
        bbrange = bbrange.first:bbrange.last
        bb_pad = max_bb_idx_size - length(string(bb_idx))
        bb_start_str = string("$(bb_idx) ",length(cfg.blocks[bb_idx].preds) <= 1 ? "─" : "┄",  "─"^(bb_pad)," ")
        if idx != last(bbrange)
            if idx == first(bbrange)
                print(io, bb_start_str)
            else
                print(io, "│  "," "^max_bb_idx_size)
            end
        end
        print_sep = false
        if idx == last(bbrange)
            print_sep = true
        end
        floop = true
        while !isempty(new_nodes_perm) && code.new_nodes[Base.peek(new_nodes_perm)][1] == idx
            node_idx = popfirst!(new_nodes_perm)
            _, typ, node, line = code.new_nodes[node_idx]
            node_idx += length(code.stmts)
            if print_sep
                if floop
                    print(io, bb_start_str)
                else
                    print(io, "│  "," "^max_bb_idx_size)
                end
            end
            print_sep = true
            floop = false
            print_ssa_typ = !isa(node, PiNode) && node_idx in used
            Base.with_output_color(:yellow, io) do io′
                print_node(io′, node_idx, node, used, maxsize; color = false,
                    print_typ=!print_ssa_typ || (isa(node, Expr) && typ != node.typ))
            end
            if print_ssa_typ
                printstyled(io, "::$(typ)", color=:red)
            end
            println(io)
        end
        if print_sep
            if idx == first(bbrange) && floop
                print(io, bb_start_str)
            else
                print(io, idx == last(bbrange) ? string("└", "─"^(1+max_bb_idx_size), " ") :
                    string("│  ", " "^max_bb_idx_size))
            end
        end
        if idx == last(bbrange)
            bb_idx += 1
        end
        typ = code.types[idx]
        print_ssa_typ = !isa(stmt, PiNode) && idx in used
        print_node(io, idx, stmt, used, maxsize,
            print_typ=!print_ssa_typ || (isa(stmt, Expr) && typ != stmt.typ))
        if print_ssa_typ
            printstyled(io, "::$(typ)", color=:red)
        end
        println(io)
    end
end
