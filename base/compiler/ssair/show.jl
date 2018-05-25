if Pair != Base.Pair
import Base: Base, IOContext, string, join, sprint
IOContext(io::IO, KV::Pair) = IOContext(io, Base.Pair(KV[1], KV[2]))
length(s::String) = Base.length(s)
^(s::String, i::Int) = Base.:^(s, i)
end
isexpr(e::Expr, s::Symbol) = e.head === s
isexpr(@nospecialize(e), s::Symbol) = false

function Base.show(io::IO, cfg::CFG)
    foreach(pairs(cfg.blocks)) do (idx, block)
        Base.println("$idx\t=>\t", join(block.succs, ", "))
    end
end

print_ssa(io::IO, val::SSAValue, argnames) = Base.print(io, "%$(val.id)")
print_ssa(io::IO, val::Argument, argnames) = Base.print(io, isempty(argnames) ? "%%$(val.n)" : "%%$(argnames[val.n])")
print_ssa(io::IO, val::GlobalRef, argnames) = Base.print(io, val)
print_ssa(io::IO, @nospecialize(val), argnames) = Base.show(io, val)

function print_node(io::IO, idx, stmt, used, argnames, maxsize; color = true, print_typ=true)
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
                    print_ssa(io′, stmt.values[i], argnames)
                end
            "$e => $v"
        end
        Base.print(io, "φ ", '(', join(args, ", "), ')')
    elseif isa(stmt, PhiCNode)
        Base.print(io, "φᶜ ", '(', join(map(x->sprint(print_ssa, x, argnames), stmt.values), ", "), ')')
    elseif isa(stmt, PiNode)
        Base.print(io, "π (")
        print_ssa(io, stmt.val, argnames)
        Base.print(io, ", ")
        if color
            Base.printstyled(io, stmt.typ, color=:cyan)
        else
            Base.print(io, stmt.typ)
        end
        Base.print(io, ")")
    elseif isa(stmt, UpsilonNode)
        Base.print(io, "ϒ (")
        isdefined(stmt, :val) ?
            print_ssa(io, stmt.val, argnames) :
            Base.print(io, "#undef")
        Base.print(io, ")")
    elseif isa(stmt, ReturnNode)
        if !isdefined(stmt, :val)
            Base.print(io, "unreachable")
        else
            Base.print(io, "return ")
            print_ssa(io, stmt.val, argnames)
        end
    elseif isa(stmt, GotoIfNot)
        Base.print(io, "goto ", stmt.dest, " if not ")
        print_ssa(io, stmt.cond, argnames)
    elseif isexpr(stmt, :call)
        print_ssa(io, stmt.args[1], argnames)
        Base.print(io, "(")
        Base.print(io, join(map(arg->sprint(io->print_ssa(io, arg, argnames)), stmt.args[2:end]), ", "))
        Base.print(io, ")")
        if print_typ && stmt.typ !== Any
            Base.print(io, "::$(stmt.typ)")
        end
    elseif isexpr(stmt, :invoke)
        print(io, "invoke ")
        linfo = stmt.args[1]
        print_ssa(io, stmt.args[2], argnames)
        Base.print(io, "(")
        sig = linfo.specTypes === Tuple ? () : Base.unwrap_unionall(linfo.specTypes).parameters
        print_arg(i) = sprint() do io
            print_ssa(io, stmt.args[2+i], argnames)
            if (i + 1) <= length(sig)
                print(io, "::$(sig[i+1])")
            end
        end
        Base.print(io, join((print_arg(i) for i=1:(length(stmt.args)-2)), ", "))
        Base.print(io, ")")
        if print_typ && stmt.typ !== Any
            Base.print(io, "::$(stmt.typ)")
        end
    elseif isexpr(stmt, :new)
        Base.print(io, "new(")
        Base.print(io, join(String[arg->sprint(io->print_ssa(io, arg, argnames)) for arg in stmt.args]), ", ")
        Base.print(io, ")")
    else
        Base.print(io, stmt)
    end
end

function compute_inlining_depth(linetable, iline)
    depth = 0
    while iline != 0
        linetable[iline].inlined_at == 0 && break
        depth += 1
        iline = linetable[iline].inlined_at
    end
    depth
end

function should_print_ssa_type(node)
    if isa(node, Expr)
        return !(node.head in (:gc_preserve_begin, :gc_preserve_end))
    end
    return !isa(node, PiNode)   && !isa(node, GotoIfNot) &&
           !isa(node, GotoNode) && !isa(node, ReturnNode)
end

function default_expr_type_printer(io, typ)
    typ_str = try
        string(typ)
    catch
        "<error_printing>"
    end
    Base.printstyled(io, "::$(typ_str)", color=:cyan)
end

function compute_loc_stack(code, line)
    stack = []
    line === 0 && return stack
    inlined_at = code.linetable[line].inlined_at
    if inlined_at != 0
        push!(stack, inlined_at)
        entry = code.linetable[inlined_at]
        while entry.inlined_at != 0
            push!(stack, entry.inlined_at)
            entry = code.linetable[entry.inlined_at]
        end
        reverse!(stack)
    end
    push!(stack, line)
end

"""
    Compute line number annotations for an IRCode

This functions compute three sets of annotations for each IR line. Take the following
example (taken from `@code_typed sin(1.0)`):

```
    **                                                    ***         **********
    35 6 ── %10  = :(Base.mul_float)(%%2, %%2)::Float64   │╻╷         sin_kernel
       │    %11  = :(Base.mul_float)(%10, %10)::Float64   ││╻          *
```

The three annotations are indicated with `*`. The first one is the line number of the
active function (printed once whenver the outer most line number changes). The second
is the inlining indicator. The number of lines indicate the level of nesting, with a
half-size line (╷) indicating the start of a scope and a full size line (│) indicating
a continuing scope. The last annotation is the most complicated one. It is a heuristic
way to print the name of the entered scope. What it attempts to do is print the outermost
scope that hasn't been printed before. Let's work a number of examples to see the impacts
and tradeoffs involved.

```
f() = leaf_function() # Delibarately not defined to end up in the IR verbatim
g() = f()
h() = g()
top_function() = h()
```

After inlining, we end up with:
```
1 1 ─ %1 = :(Main.leaf_function)()::Any   │╻╷╷ h
  └──      return %1                      │
```

We see that the only function printed is the outermost function. This certainly loses
some information, but the idea is that the outermost function would have the most
semantic meaning (in the context of the function we're looking at).

On the other hand, let's see what happens when we redefine f:
```
function f()
    leaf_function()
    leaf_function()
    leaf_function()
end
```

We get:
```
1 1 ─      :(Main.leaf_function)()::Any   │╻╷╷ h
  │        :(Main.leaf_function)()::Any   ││┃│  g
  │   %3 = :(Main.leaf_function)()::Any   │││┃   f
  └──      return %3                      │
```

Even though we were in the `f` scope since the first statement, it tooks us two statements
to catch up and print the intermediate scopes. Which scope is printed is indicated both
by the indentation of the method name and by an increased thickness of the appropriate
line for the scope.
"""
function compute_ir_line_annotations(code::IRCode)
    loc_annotations = String[]
    loc_methods = String[]
    loc_lineno = String[]
    cur_group = 1
    last_line = 0
    last_lineno = 0
    last_stack = []
    last_printed_depth = 0
    for idx in eachindex(code.stmts)
        buf = IOBuffer()
        line = code.lines[idx]
        depth = compute_inlining_depth(code.linetable, line)
        iline = line
        lineno = 0
        loc_method = ""
        print(buf, "│")
        if line !== 0
            stack = compute_loc_stack(code, line)
            lineno = code.linetable[stack[1]].line
            x = min(length(last_stack), length(stack))
            if length(stack) != 0
                # Compute the last depth that was in common
                first_mismatch = findfirst(i->last_stack[i] != stack[i], 1:x)
                # If the first mismatch is the last stack frame, that might just
                # be a line number mismatch in inner most frame. Ignore those
                if length(last_stack) == length(stack) && first_mismatch == length(stack)
                    last_entry, entry = code.linetable[last_stack[end]], code.linetable[stack[end]]
                    if last_entry.method == entry.method && last_entry.file == entry.file
                        first_mismatch = nothing
                    end
                end
                last_depth = coalesce(first_mismatch, x+1)-1
                if min(depth, last_depth) > last_printed_depth
                    printing_depth = min(depth, last_printed_depth + 1)
                    last_printed_depth = printing_depth
                elseif length(stack) > length(last_stack) || first_mismatch != nothing
                    printing_depth = min(depth, last_depth + 1)
                    last_printed_depth = printing_depth
                else
                    printing_depth = 0
                end
                stole_one = false
                if printing_depth != 0
                    for _ in 1:(printing_depth-1)
                        print(buf, "│")
                    end
                    if printing_depth <= last_depth-1 && first_mismatch === nothing
                        print(buf, "┃")
                        for _ in printing_depth+1:min(depth, last_depth)
                            print(buf, "│")
                        end
                    else
                        stole_one = true
                        print(buf, "╻")
                    end
                else
                    for _ in 1:min(depth, last_depth)
                        print(buf, "│")
                    end
                end
                print(buf, "╷"^max(0,depth-last_depth-stole_one))
                if printing_depth != 0
                    if length(stack) == printing_depth
                        loc_method = String(code.linetable[line].method)
                    else
                        loc_method = String(code.linetable[stack[printing_depth+1]].method)
                    end
                end
                loc_method = string(" "^printing_depth, loc_method)
            end
            last_stack = stack
            entry = code.linetable[line]
        end
        push!(loc_annotations, String(take!(buf)))
        push!(loc_lineno, (lineno != 0 && lineno != last_lineno) ? string(lineno) : "")
        push!(loc_methods, loc_method)
        last_line = line
        (lineno != 0) && (last_lineno = lineno)
    end
    (loc_annotations, loc_methods, loc_lineno)
end

Base.show(io::IO, code::IRCode) = show_ir(io, code)
function show_ir(io::IO, code::IRCode, expr_type_printer=default_expr_type_printer; argnames=Symbol[], verbose_linetable=false)
    (lines, cols) = displaysize(io)
    used = IdSet{Int}()
    foreach(stmt->scan_ssa_use!(push!, used, stmt), code.stmts)
    cfg = code.cfg
    max_bb_idx_size = length(string(length(cfg.blocks)))
    bb_idx = 1
    if any(i->!isassigned(code.new_nodes, i), 1:length(code.new_nodes))
        printstyled(io, :red, "ERROR: New node array has unset entry\n")
    end
    new_nodes = code.new_nodes[filter(i->isassigned(code.new_nodes, i), 1:length(code.new_nodes))]
    foreach(nn -> scan_ssa_use!(push!, used, nn.node), new_nodes)
    perm = sortperm(new_nodes, by = x->x.pos)
    new_nodes_perm = Iterators.Stateful(perm)

    if isempty(used)
        maxsize = 0
    else
        maxused = maximum(used)
        maxsize = length(string(maxused))
    end
    if !verbose_linetable
        (loc_annotations, loc_methods, loc_lineno) = compute_ir_line_annotations(code)
        max_loc_width = maximum(length(str) for str in loc_annotations)
        max_lineno_width = maximum(length(str) for str in loc_lineno)
        max_method_width = maximum(length(str) for str in loc_methods)
    end
    max_depth = maximum(line == 0 ? 1 : compute_inlining_depth(code.linetable, line) for line in code.lines)
    last_stack = []
    for idx in eachindex(code.stmts)
        if !isassigned(code.stmts, idx)
            # This is invalid, but do something useful rather
            # than erroring, to make debugging easier
            printstyled(io, :red, "UNDEF\n")
            continue
        end
        stmt = code.stmts[idx]
        # Compute BB guard rail
        bbrange = cfg.blocks[bb_idx].stmts
        bbrange = bbrange.first:bbrange.last
        bb_pad = max_bb_idx_size - length(string(bb_idx))
        bb_start_str = string("$(bb_idx) ",length(cfg.blocks[bb_idx].preds) <= 1 ? "─" : "┄",  "─"^(bb_pad)," ")
        bb_guard_rail_cont = string("│  "," "^max_bb_idx_size)
        if idx == first(bbrange)
            bb_guard_rail = bb_start_str
        else
            bb_guard_rail = bb_guard_rail_cont
        end
        # Print linetable information
        if verbose_linetable
            stack = compute_loc_stack(code, code.lines[idx])
            # We need to print any stack frames that did not exist in the last stack
            ndepth = max(1, length(stack))
            rail = string(" "^(max_depth+1-ndepth), "│"^ndepth)
            start_column = cols - max_depth - 10
            for (i, x) in enumerate(stack)
                if i > length(last_stack) || last_stack[i] != x
                    entry = code.linetable[x]
                    printstyled(io, "\e[$(start_column)G$(rail)\e[1G", color = :light_black)
                    print(io, bb_guard_rail)
                    ssa_guard = " "^(maxsize+4+(i-1))
                    entry_label = "$(ssa_guard)$(entry.method) at $(entry.file):$(entry.line) "
                    hline = string("─"^(start_column-length(entry_label)-length(bb_guard_rail)+max_depth-i), "┐")
                    printstyled(io, string(entry_label, hline), "\n"; color=:light_black)
                    bb_guard_rail = bb_guard_rail_cont
                end
            end
            printstyled(io, "\e[$(start_column)G$(rail)\e[1G", color = :light_black)
            last_stack = stack
        else
            annotation = loc_annotations[idx]
            loc_method = loc_methods[idx]
            lineno = loc_lineno[idx]
            # Print location information right aligned. If the line below is too long, it'll overwrite this,
            # but that's what we want.
            if get(io, :color, false)
                method_start_column = cols - max_method_width - max_loc_width - 2
                filler = " "^(max_loc_width-length(annotation))
                printstyled(io, "\e[$(method_start_column)G$(annotation)$(filler)$(loc_method)\e[1G", color = :light_black)
            end
            printstyled(io, lineno, " "^(max_lineno_width-length(lineno)+1); color = :light_black)
        end
        idx != last(bbrange) && Base.print(io, bb_guard_rail)
        print_sep = false
        if idx == last(bbrange)
            print_sep = true
        end
        floop = true
        while !isempty(new_nodes_perm) && new_nodes[peek(new_nodes_perm)].pos == idx
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
            Base.with_output_color(:yellow, io) do io′
                print_node(io′, node_idx, new_node.node, used, argnames, maxsize; color = false, print_typ=false)
            end
            if should_print_ssa_type(new_node.node) && node_idx in used
                expr_type_printer(io, new_node.typ)
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
        try
            print_node(io, idx, stmt, used, argnames, maxsize, print_typ=false)
        catch e
            print(io, "<error printing>")
        end
        if should_print_ssa_type(stmt) && idx in used
            expr_type_printer(io, typ)
        end
        println(io)
    end
end
