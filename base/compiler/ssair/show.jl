if Pair != Base.Pair
import Base: Base, IOContext, string, join, sprint
IOContext(io::IO, KV::Pair) = IOContext(io, Base.Pair(KV[1], KV[2]))
length(s::String) = Base.length(s)
^(s::String, i::Int) = Base.:^(s, i)
end

function Base.show(io::IO, cfg::CFG)
    foreach(pairs(cfg.blocks)) do (idx, block)
        Base.println("$idx\t=>\t", join(block.succs, ", "))
    end
end

print_ssa(io::IO, val) = isa(val, SSAValue) ? Base.print(io, "%$(val.id)") :
                         isa(val, Argument) ? Base.print(io, "%%$(val.n)") : Base.print(io, val)
function print_node(io::IO, idx, stmt, used, maxsize; color = true, print_typ=true)
    if idx in used
        pad = " "^(maxsize-length(string(idx)))
        Base.print(io, "%$idx $pad= ")
    else
        Base.print(io, " "^(maxsize+4))
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
        Base.print(io, "φ ", '(', join(args, ", "), ')')
    elseif isa(stmt, PhiCNode)
        Base.print(io, "φᶜ ", '(', join(map(x->sprint(print_ssa, x), stmt.values), ", "), ')')
    elseif isa(stmt, PiNode)
        Base.print(io, "π (")
        print_ssa(io, stmt.val)
        Base.print(io, ", ")
        if color
            Base.printstyled(io, stmt.typ, color=:red)
        else
            Base.print(io, stmt.typ)
        end
        Base.print(io, ")")
    elseif isa(stmt, UpsilonNode)
        Base.print(io, "ϒ (")
        isdefined(stmt, :val) ?
            print_ssa(io, stmt.val) :
            Base.print(io, "#undef")
        Base.print(io, ")")
    elseif isa(stmt, ReturnNode)
        if !isdefined(stmt, :val)
            Base.print(io, "unreachable")
        else
            Base.print(io, "return ")
            print_ssa(io, stmt.val)
        end
    elseif isa(stmt, GotoIfNot)
        Base.print(io, "goto ", stmt.dest, " if not ")
        print_ssa(io, stmt.cond)
    elseif isexpr(stmt, :call)
        print_ssa(io, stmt.args[1])
        Base.print(io, "(")
        Base.print(io, join(map(arg->sprint(io->print_ssa(io, arg)), stmt.args[2:end]), ", "))
        Base.print(io, ")")
        if print_typ && stmt.typ !== Any
            Base.print(io, "::$(stmt.typ)")
        end
    elseif isexpr(stmt, :new)
        Base.print(io, "new(")
        Base.print(io, join(map(arg->sprint(io->print_ssa(io, arg)), stmt.args), ", "))
        Base.print(io, ")")
    else
        Base.print(io, stmt)
    end
end

function Base.show(io::IO, code::IRCode)
    io = IOContext(io, :color=>true)
    used = IdSet{Int}()
    Base.println(io, "Code")
    foreach(stmt->scan_ssa_use!(used, stmt), code.stmts)
    cfg = code.cfg
    max_bb_idx_size = length(string(length(cfg.blocks)))
    bb_idx = 1
    if any(i->!isassigned(code.new_nodes, i), 1:length(code.new_nodes))
        printstyled(io, :red, "ERROR: New node array has unset entry\n")
    end
    new_nodes = code.new_nodes[filter(i->isassigned(code.new_nodes, i), 1:length(code.new_nodes))]
    foreach(nn -> scan_ssa_use!(used, nn.node), new_nodes)
    perm = sortperm(new_nodes, by = x->x[1])
    new_nodes_perm = Iterators.Stateful(perm)

    if isempty(used)
        maxsize = 0
    else
        maxused = maximum(used)
        maxsize = length(string(maxused))
    end

    for idx in eachindex(code.stmts)
        if !isassigned(code.stmts, idx)
            # This is invalid, but do something useful rather
            # than erroring, to make debugging easier
            printstyled(io, :red, "UNDEF\n")
            continue
        end
        stmt = code.stmts[idx]
        bbrange = cfg.blocks[bb_idx].stmts
        bbrange = bbrange.first:bbrange.last
        bb_pad = max_bb_idx_size - length(string(bb_idx))
        bb_start_str = string("$(bb_idx) ",length(cfg.blocks[bb_idx].preds) <= 1 ? "─" : "┄",  "─"^(bb_pad)," ")
        if idx != last(bbrange)
            if idx == first(bbrange)
                Base.print(io, bb_start_str)
            else
                Base.print(io, "│  "," "^max_bb_idx_size)
            end
        end
        print_sep = false
        if idx == last(bbrange)
            print_sep = true
        end
        floop = true
        while !isempty(new_nodes_perm) && new_nodes[peek(new_nodes_perm)][1] == idx
            node_idx = popfirst!(new_nodes_perm)
            new_node = new_nodes[node_idx]
            node_idx += length(code.stmts)
            if print_sep
                if floop
                    Base.print(io, bb_start_str)
                else
                    Base.print(io, "│  "," "^max_bb_idx_size)
                end
            end
            print_sep = true
            floop = false
            print_ssa_typ = !isa(new_node.node, PiNode) && node_idx in used
            Base.with_output_color(:yellow, io) do io′
                print_node(io′, node_idx, new_node.node, used, maxsize; color = false,
                    print_typ=!print_ssa_typ || (isa(new_node.node, Expr) && typ != new_node.node.typ))
            end
            if print_ssa_typ
                Base.printstyled(io, "::$(new_node.typ)", color=:red)
            end
            Base.println(io)
        end
        if print_sep
            if idx == first(bbrange) && floop
                Base.print(io, bb_start_str)
            else
                Base.print(io, idx == last(bbrange) ? string("└", "─"^(1+max_bb_idx_size), " ") :
                    string("│  ", " "^max_bb_idx_size))
            end
        end
        if idx == last(bbrange)
            bb_idx += 1
        end
        if !isassigned(code.types, idx)
            # Again, this is an error, but can happen if passes don't update their type information
            printstyled(io, "::UNDEF", color=:red)
            println(io)
            continue
        end
        typ = code.types[idx]
        print_ssa_typ = !isa(stmt, PiNode) && idx in used
        print_node(io, idx, stmt, used, maxsize,
            print_typ=!print_ssa_typ || (isa(stmt, Expr) && typ != stmt.typ))
        if print_ssa_typ
            Base.printstyled(io, "::$(typ)", color=:red)
        end
        Base.println(io)
    end
end
