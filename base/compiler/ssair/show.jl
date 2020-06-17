# This file is a part of Julia. License is MIT: https://julialang.org/license

@nospecialize

if Pair != Base.Pair
import Base: Base, IOContext, string, join, sprint
IOContext(io::IO, KV::Pair) = IOContext(io, Base.Pair(KV[1], KV[2]))
length(s::String) = Base.length(s)
^(s::String, i::Int) = Base.:^(s, i)
end

import Base: show_unquoted
using Base: printstyled, with_output_color, prec_decl

function Base.show(io::IO, cfg::CFG)
    for (idx, block) in enumerate(cfg.blocks)
        print(io, idx, "\t=>\t")
        join(io, block.succs, ", ")
        println(io)
    end
end

function print_stmt(io::IO, idx::Int, @nospecialize(stmt), used::BitSet, maxlength_idx::Int, color::Bool, show_type::Bool)
    if idx in used
        idx_s = string(idx)
        pad = " "^(maxlength_idx - length(idx_s) + 1)
        print(io, "%", idx_s, pad, "= ")
    else
        print(io, " "^(maxlength_idx + 4))
    end
    # TODO: `indent` is supposed to be the full width of the leader for correct alignment
    indent = 16
    if !color && stmt isa PiNode
        # when the outer context is already colored (green, for pending nodes), don't use the usual coloring printer
        print(io, "π (")
        show_unquoted(io, stmt.val, indent)
        print(io, ", ")
        print(io, stmt.typ)
        print(io, ")")
    elseif isexpr(stmt, :invoke)
        # TODO: why is this here, and not in Base.show_unquoted
        print(io, "invoke ")
        linfo = stmt.args[1]
        show_unquoted(io, stmt.args[2], indent)
        print(io, "(")
        # XXX: this is wrong if `sig` is not a concretetype method
        # more correct would be to use `fieldtype(sig, i)`, but that would obscure / discard Varargs information in show
        sig = linfo.specTypes == Tuple ? Core.svec() : Base.unwrap_unionall(linfo.specTypes).parameters::Core.SimpleVector
        print_arg(i) = sprint() do io
            show_unquoted(io, stmt.args[i], indent)
            if (i - 1) <= length(sig)
                print(io, "::", sig[i - 1])
            end
        end
        join(io, (print_arg(i) for i = 3:length(stmt.args)), ", ")
        print(io, ")")
    # given control flow information, we prefer to print these with the basic block #, instead of the ssa %
    elseif isexpr(stmt, :enter) && length(stmt.args) == 1 && stmt.args[1] isa Int
        print(io, "\$(Expr(:enter, #", stmt.args[1]::Int, "))")
    elseif stmt isa GotoNode
        print(io, "goto #", stmt.label)
    elseif stmt isa PhiNode
        show_unquoted_phinode(io, stmt, indent, "#")
    elseif stmt isa GotoIfNot
        show_unquoted_gotoifnot(io, stmt, indent, "#")
    # everything else in the IR, defer to the generic AST printer
    else
        show_unquoted(io, stmt, indent, show_type ? prec_decl : 0)
    end
    nothing
end

show_unquoted(io::IO, val::Argument, indent::Int, prec::Int) = show_unquoted(io, Core.SlotNumber(val.n), indent, prec)

show_unquoted(io::IO, stmt::PhiNode, indent::Int, ::Int) = show_unquoted_phinode(io, stmt, indent, "%")
function show_unquoted_phinode(io::IO, stmt::PhiNode, indent::Int, prefix::String)
    args = map(1:length(stmt.edges)) do i
        e = stmt.edges[i]
        v = !isassigned(stmt.values, i) ? "#undef" :
            sprint() do io′
                show_unquoted(io′, stmt.values[i], indent)
            end
        return "$prefix$e => $v"
    end
    print(io, "φ ", '(')
    join(io, args, ", ")
    print(io, ')')
end

function show_unquoted(io::IO, stmt::PhiCNode, indent::Int, ::Int)
    print(io, "φᶜ (")
    first = true
    for v in stmt.values
        first ? (first = false) : print(io, ", ")
        show_unquoted(io, v, indent)
    end
    print(io, ")")
end

function show_unquoted(io::IO, stmt::PiNode, indent::Int, ::Int)
    print(io, "π (")
    show_unquoted(io, stmt.val, indent)
    print(io, ", ")
    printstyled(io, stmt.typ, color=:cyan)
    print(io, ")")
end

function show_unquoted(io::IO, stmt::UpsilonNode, indent::Int, ::Int)
    print(io, "ϒ (")
    isdefined(stmt, :val) ?
        show_unquoted(io, stmt.val, indent) :
        print(io, "#undef")
    print(io, ")")
end

function show_unquoted(io::IO, stmt::ReturnNode, indent::Int, ::Int)
    if !isdefined(stmt, :val)
        print(io, "unreachable")
    else
        print(io, "return ")
        show_unquoted(io, stmt.val, indent)
    end
end

show_unquoted(io::IO, stmt::GotoIfNot, indent::Int, ::Int) = show_unquoted_gotoifnot(io, stmt, indent, "%")
function show_unquoted_gotoifnot(io::IO, stmt::GotoIfNot, indent::Int, prefix::String)
    print(io, "goto ", prefix, stmt.dest, " if not ")
    show_unquoted(io, stmt.cond, indent)
end

function compute_inlining_depth(linetable::Vector, iline::Int32)
    iline == 0 && return 1
    depth = -1
    while iline != 0
        depth += 1
        lineinfo = linetable[iline]::LineInfoNode
        iline = lineinfo.inlined_at
    end
    return depth
end

function should_print_ssa_type(@nospecialize node)
    if isa(node, Expr)
        return !(node.head in (:gc_preserve_begin, :gc_preserve_end, :meta, :return, :enter, :leave))
    end
    return !isa(node, PiNode)   && !isa(node, GotoIfNot) &&
           !isa(node, GotoNode) && !isa(node, ReturnNode) &&
           !isa(node, QuoteNode)
end

function default_expr_type_printer(io::IO, @nospecialize(typ), used::Bool)
    printstyled(io, "::", typ, color=(used ? :cyan : :light_black))
    nothing
end

normalize_method_name(m::Method) = m.name
normalize_method_name(m::MethodInstance) = (m.def::Method).name
normalize_method_name(m::Symbol) = m
normalize_method_name(m) = Symbol("")
@noinline method_name(m::LineInfoNode) = normalize_method_name(m.method)

# converts the linetable for line numbers
# into a list in the form:
#   1 outer-most-frame
#   2   inlined-frame
#   3     innermost-frame
function compute_loc_stack(linetable::Vector, line::Int32)
    stack = Int[]
    while line != 0
        entry = linetable[line]::LineInfoNode
        pushfirst!(stack, line)
        line = entry.inlined_at
    end
    return stack
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
    linetable = code.linetable
    lines = code.stmts.line
    for idx in 1:length(lines)
        buf = IOBuffer()
        line = lines[idx]
        print(buf, "│")
        depth = compute_inlining_depth(linetable, line)
        iline = line
        lineno = 0
        loc_method = ""
        if line != 0
            stack = compute_loc_stack(linetable, line)
            lineno = linetable[stack[1]].line
            x = min(length(last_stack), length(stack))
            if length(stack) != 0
                # Compute the last depth that was in common
                first_mismatch = findfirst(i->last_stack[i] != stack[i], 1:x)
                # If the first mismatch is the last stack frame, that might just
                # be a line number mismatch in inner most frame. Ignore those
                if length(last_stack) == length(stack) && first_mismatch == length(stack)
                    last_entry, entry = linetable[last_stack[end]], linetable[stack[end]]
                    if method_name(last_entry) === method_name(entry) && last_entry.file === entry.file
                        first_mismatch = nothing
                    end
                end
                last_depth = something(first_mismatch, x+1)-1
                if min(depth, last_depth) > last_printed_depth
                    printing_depth = min(depth, last_printed_depth + 1)
                    last_printed_depth = printing_depth
                elseif length(stack) > length(last_stack) || first_mismatch !== nothing
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
                print(buf, "╷"^max(0, depth - last_depth - stole_one))
                if printing_depth != 0
                    if length(stack) == printing_depth
                        loc_method = line
                    else
                        loc_method = stack[printing_depth + 1]
                    end
                    loc_method = method_name(linetable[loc_method])
                end
                loc_method = string(" "^printing_depth, loc_method)
            end
            last_stack = stack
            entry = linetable[line]
        end
        push!(loc_annotations, String(take!(buf)))
        push!(loc_lineno, (lineno != 0 && lineno != last_lineno) ? string(lineno) : "")
        push!(loc_methods, loc_method)
        last_line = line
        (lineno != 0) && (last_lineno = lineno)
    end
    return (loc_annotations, loc_methods, loc_lineno)
end

Base.show(io::IO, code::IRCode) = show_ir(io, code)


lineinfo_disabled(io::IO, linestart::String, lineidx::Int32) = ""

function DILineInfoPrinter(linetable::Vector, showtypes::Bool=false)
    context = LineInfoNode[]
    context_depth = Ref(0)
    indent(s::String) = s^(max(context_depth[], 1) - 1)
    function emit_lineinfo_update(io::IO, linestart::String, lineidx::Int32)
        # internal configuration options:
        linecolor = :yellow
        collapse = showtypes ? false : true
        indent_all = true
        # convert lineidx to a vector
        if lineidx == typemin(Int32)
            # sentinel value: reset internal (and external) state
            pops = indent("└")
            if !isempty(pops)
                print(io, linestart)
                printstyled(io, pops; color=linecolor)
                println(io)
            end
            empty!(context)
            context_depth[] = 0
        elseif lineidx > 0 # just skip over lines with no debug info at all
            DI = LineInfoNode[]
            while lineidx != 0
                entry = linetable[lineidx]::LineInfoNode
                push!(DI, entry)
                lineidx = entry.inlined_at
            end
            # FOR DEBUGGING, or if you just like very excessive output:
            # this prints out the context in full for every statement
            #empty!(context)
            #context_depth[] = 0
            nframes = length(DI)
            nctx = 0
            pop_skips = 0
            # compute the size of the matching prefix in the inlining information stack
            for i = 1:min(length(context), nframes)
                CtxLine = context[i]
                FrameLine = DI[nframes - i + 1]
                CtxLine === FrameLine || break
                nctx = i
            end
            update_line_only = false
            if collapse && 0 < nctx
                # check if we're adding more frames with the same method name,
                # if so, drop all existing calls to it from the top of the context
                # AND check if instead the context was previously printed that way
                # but now has removed the recursive frames
                let method = method_name(context[nctx])
                    if (nctx < nframes && method_name(DI[nframes - nctx]) === method) ||
                       (nctx < length(context) && method_name(context[nctx + 1]) === method)
                        update_line_only = true
                        while nctx > 0 && method_name(context[nctx]) === method
                            nctx -= 1
                        end
                    end
                end
            end
            # examine what frames we're returning from
            if nctx < length(context)
                # compute the new inlining depth
                if collapse
                    npops = 1
                    let Prev = method_name(context[nctx + 1])
                        for i = (nctx + 2):length(context)
                            Next = method_name(context[i])
                            Prev === Next || (npops += 1)
                            Prev = Next
                        end
                    end
                else
                    npops = length(context) - nctx
                end
                # look at the first non-matching element to see if we are only changing the line number
                if !update_line_only && nctx < nframes
                    let CtxLine = context[nctx + 1],
                        FrameLine = DI[nframes - nctx]
                        if CtxLine.file === FrameLine.file &&
                                method_name(CtxLine) === method_name(FrameLine)
                            update_line_only = true
                        end
                    end
                end
                resize!(context, nctx)
                update_line_only && (npops -= 1)
                if npops > 0
                    context_depth[] -= npops
                    print(io, linestart)
                    printstyled(io, indent("│"), "└"^npops; color=linecolor)
                    println(io)
                end
            end
            # see what change we made to the outermost line number
            if update_line_only
                frame = DI[nframes - nctx]
                nctx += 1
                push!(context, frame)
                if frame.line != typemax(frame.line) && frame.line != 0
                    print(io, linestart)
                    Base.with_output_color(linecolor, io) do io
                        print(io, indent("│"), " @ ", frame.file, ":", frame.line, " within `", method_name(frame), "'")
                        if collapse
                            method = method_name(frame)
                            while nctx < nframes
                                frame = DI[nframes - nctx]
                                method_name(frame) === method || break
                                nctx += 1
                                push!(context, frame)
                                print(io, " @ ", frame.file, ":", frame.line)
                            end
                        end
                    end
                    println(io)
                end
            end
            # now print the rest of the new frames
            while nctx < nframes
                frame = DI[nframes - nctx]
                nctx += 1
                started = false
                if showtypes && !isa(frame.method, Symbol) && nctx != 1
                    print(io, linestart)
                    Base.with_output_color(linecolor, io) do io
                        print(io, indent("│"))
                        print(io, "┌ invoke ", frame.method)
                        println(io)
                    end
                    started = true
                end
                print(io, linestart)
                Base.with_output_color(linecolor, io) do io
                    print(io, indent("│"))
                    push!(context, frame)
                    context_depth[] += 1
                    nctx != 1 && print(io, started ? "│" : "┌")
                    print(io, " @ ", frame.file)
                    if frame.line != typemax(frame.line) && frame.line != 0
                        print(io, ":", frame.line)
                    end
                    print(io, " within `", method_name(frame), "'")
                    if collapse
                        method = method_name(frame)
                        while nctx < nframes
                            frame = DI[nframes - nctx]
                            method_name(frame) === method || break
                            nctx += 1
                            push!(context, frame)
                            print(io, " @ ", frame.file, ":", frame.line)
                        end
                    end
                end
                println(io)
            end
            # FOR DEBUGGING `collapse`:
            # this double-checks the computation of context_depth
            #let Prev = method_name(context[1]),
            #    depth2 = 1
            #    for i = 2:nctx
            #        Next = method_name(context[i])
            #        (collapse && Prev === Next) || (depth2 += 1)
            #        Prev = Next
            #    end
            #    @assert context_depth[] == depth2
            #end
        end
        indent_all || return ""
        return sprint(io -> printstyled(io, indent("│"), color=linecolor), context=io)
    end
    return emit_lineinfo_update
end


function show_ir(io::IO, code::IRCode, expr_type_printer=default_expr_type_printer; verbose_linetable=false)
    cols = displaysize(io)[2]
    used = BitSet()
    stmts = code.stmts
    isempty(stmts) && return # unlikely, but avoid errors from reducing over empty sets
    cfg = code.cfg
    max_bb_idx_size = length(string(length(cfg.blocks)))
    new_nodes = code.new_nodes.stmts
    new_nodes_info = code.new_nodes.info
    bb_idx = 1
    for stmt in stmts
        scan_ssa_use!(push!, used, stmt[:inst])
    end
    if any(i -> !isassigned(new_nodes.inst, i), 1:length(new_nodes))
        printstyled(io, "ERROR: New node array has unset entry\n", color=:red)
        new_nodes_perm = filter(i -> isassigned(new_nodes.inst, i), 1:length(new_nodes))
    else
        new_nodes_perm = collect(1:length(new_nodes))
    end
    for nn in new_nodes_perm
        scan_ssa_use!(push!, used, new_nodes[nn][:inst])
    end
    sort!(new_nodes_perm, by = x -> (x = new_nodes_info[x]; (x.pos, x.attach_after)))
    perm_idx = 1

    if isempty(used)
        maxlength_idx = 0
    else
        maxused = maximum(used)
        maxlength_idx = length(string(maxused))
    end
    if !verbose_linetable
        (loc_annotations, loc_methods, loc_lineno) = compute_ir_line_annotations(code)
        max_loc_width = maximum(length(str) for str in loc_annotations)
        max_lineno_width = maximum(length(str) for str in loc_lineno)
        max_method_width = maximum(length(str) for str in loc_methods)
    end
    max_depth = maximum(compute_inlining_depth(code.linetable, stmts[i][:line]) for i in 1:length(stmts.line))
    last_stack = []
    for idx in 1:length(stmts)
        if !isassigned(stmts.inst, idx)
            # This is invalid, but do something useful rather
            # than erroring, to make debugging easier
            printstyled(io, "#UNDEF\n", color=:red)
            continue
        end
        stmt = stmts[idx]
        # Compute BB guard rail
        if bb_idx > length(cfg.blocks)
            # Even if invariants are violated, try our best to still print
            bbrange = (length(cfg.blocks) == 0 ? 1 : last(cfg.blocks[end].stmts) + 1):typemax(Int)
            bb_idx_str = "!"
            bb_type = "─"
        else
            bbrange = cfg.blocks[bb_idx].stmts
            bbrange = bbrange.start:bbrange.stop
            bb_idx_str = string(bb_idx)
            bb_type = length(cfg.blocks[bb_idx].preds) <= 1 ? "─" : "┄"
        end
        bb_pad = max_bb_idx_size - length(bb_idx_str)
        bb_start_str = string(bb_idx_str, " ", bb_type, "─"^bb_pad, " ")
        bb_guard_rail_cont = string("│  ", " "^max_bb_idx_size)
        if idx == first(bbrange)
            bb_guard_rail = bb_start_str
        else
            bb_guard_rail = bb_guard_rail_cont
        end
        # Print linetable information
        if verbose_linetable
            stack = compute_loc_stack(code.linetable, stmt[:line])
            # We need to print any stack frames that did not exist in the last stack
            ndepth = max(1, length(stack))
            rail = string(" "^(max_depth+1-ndepth), "│"^ndepth)
            start_column = cols - max_depth - 10
            for (i, x) in enumerate(stack)
                if i > length(last_stack) || last_stack[i] != x
                    entry = code.linetable[x]
                    printstyled(io, "\e[$(start_column)G$(rail)\e[1G", color = :light_black)
                    print(io, bb_guard_rail)
                    ssa_guard = " "^(maxlength_idx + 4 + (i - 1))
                    entry_label = "$(ssa_guard)$(method_name(entry)) at $(entry.file):$(entry[:line]) "
                    hline = string("─"^(start_column-length(entry_label)-length(bb_guard_rail)+max_depth-i), "┐")
                    printstyled(io, string(entry_label, hline), "\n"; color=:light_black)
                    bb_guard_rail = bb_guard_rail_cont
                end
            end
            printstyled(io, "\e[$(start_column)G$(rail)\e[1G", color = :light_black)
            last_stack = stack
        else
            if idx <= length(loc_annotations)
                # N.B.: The line array length not matching is invalid,
                # but let's be robust here
                annotation = loc_annotations[idx]
                loc_method = loc_methods[idx]
                lineno = loc_lineno[idx]
            else
                annotation = "!"
                loc_method = ""
                lineno = ""
            end
            # Print location information right aligned. If the line below is too long, it'll overwrite this,
            # but that's what we want.
            if get(io, :color, false)
                method_start_column = cols - max_method_width - max_loc_width - 2
                filler = " "^(max_loc_width-length(annotation))
                printstyled(io, "\e[$(method_start_column)G$(annotation)$(filler)$(loc_method)\e[1G", color = :light_black)
            end
            printstyled(io, lineno, " "^(max_lineno_width - length(lineno) + 1); color = :light_black)
        end
        idx != last(bbrange) && print(io, bb_guard_rail)
        print_sep = false
        if idx == last(bbrange)
            print_sep = true
        end
        floop = true
        # print new nodes first in the right position
        while perm_idx <= length(new_nodes_perm)
            node_idx = new_nodes_perm[perm_idx]
            if new_nodes_info[node_idx].pos != idx
                break
            end
            perm_idx += 1
            if !floop && !verbose_linetable
                print(io, " "^(max_lineno_width + 1))
            end
            if print_sep
                if idx == first(bbrange) && floop
                    print(io, bb_start_str)
                else
                    print(io, "│  ", " "^max_bb_idx_size)
                end
            end
            print_sep = true
            floop = false
            new_node = new_nodes[node_idx]
            node_idx += length(stmts)
            show_type = should_print_ssa_type(new_node[:inst])
            with_output_color(:green, io) do io′
                print_stmt(io′, node_idx, new_node[:inst], used, maxlength_idx, false, show_type)
            end
            if !isassigned(stmts.type, idx) # try to be robust against errors
                printstyled(io, "::#UNDEF", color=:red)
            elseif show_type
                expr_type_printer(io, new_node[:type], node_idx in used)
            end
            println(io)
        end
        if !floop && !verbose_linetable
            print(io, " "^(max_lineno_width + 1))
        end
        if print_sep
            if idx == first(bbrange) && floop
                print(io, bb_start_str)
            elseif idx == last(bbrange)
                print(io, "└", "─"^(1 + max_bb_idx_size), " ")
            else
                print(io, "│  ", " "^max_bb_idx_size)
            end
        end
        if idx == last(bbrange)
            bb_idx += 1
        end
        show_type = should_print_ssa_type(stmt[:inst])
        print_stmt(io, idx, stmt[:inst], used, maxlength_idx, true, show_type)
        if !isassigned(stmts.type, idx) # try to be robust against errors
            printstyled(io, "::#UNDEF", color=:red)
        elseif show_type
            expr_type_printer(io, stmt[:type], idx in used)
        end
        println(io)
    end
end

# Show a single statement, code.code[idx], in the context of the whole CodeInfo.
# Returns the updated value of bb_idx.
function show_ir_stmt(io::IO, code::CodeInfo, idx::Int, line_info_preprinter, line_info_postprinter, used::BitSet, cfg::CFG, bb_idx::Int)
    ds = get(io, :displaysize, (24, 80))::Tuple{Int,Int}
    cols = ds[2]
    stmts = code.code
    types = code.ssavaluetypes
    max_bb_idx_size = length(string(length(cfg.blocks)))

    if isempty(used)
        maxlength_idx = 0
    else
        maxused = maximum(used)
        maxlength_idx = length(string(maxused))
    end

    if !isassigned(stmts, idx)
        # This is invalid, but do something useful rather
        # than erroring, to make debugging easier
        printstyled(io, "#UNDEF\n", color=:red)
        return bb_idx
    end
    stmt = stmts[idx]
    # Compute BB guard rail
    if bb_idx > length(cfg.blocks)
        # If invariants are violated, print a special leader
        linestart = " "^(max_bb_idx_size + 2) # not inside a basic block bracket
        inlining_indent = line_info_preprinter(io, linestart, code.codelocs[idx])
        printstyled(io, "!!! ", "─"^max_bb_idx_size, color=:light_black)
    else
        bbrange = cfg.blocks[bb_idx].stmts
        bbrange = bbrange.start:bbrange.stop
        # Print line info update
        linestart = idx == first(bbrange) ? "  " : sprint(io -> printstyled(io, "│ ", color=:light_black), context=io)
        linestart *= " "^max_bb_idx_size
        inlining_indent = line_info_preprinter(io, linestart, code.codelocs[idx])
        if idx == first(bbrange)
            bb_idx_str = string(bb_idx)
            bb_pad = max_bb_idx_size - length(bb_idx_str)
            bb_type = length(cfg.blocks[bb_idx].preds) <= 1 ? "─" : "┄"
            printstyled(io, bb_idx_str, " ", bb_type, "─"^bb_pad, color=:light_black)
        elseif idx == last(bbrange) # print separator
            printstyled(io, "└", "─"^(1 + max_bb_idx_size), color=:light_black)
        else
            printstyled(io, "│ ", " "^max_bb_idx_size, color=:light_black)
        end
        if idx == last(bbrange)
            bb_idx += 1
        end
    end
    print(io, inlining_indent, " ")
    # convert statement index to labels, as expected by print_stmt
    if stmt isa Expr
        if stmt.head === :gotoifnot && length(stmt.args) == 2 && stmt.args[2] isa Int
            stmt = GotoIfNot(stmt.args[1], block_for_inst(cfg, stmt.args[2]::Int))
        elseif stmt.head === :enter && length(stmt.args) == 1 && stmt.args[1] isa Int
            stmt = Expr(:enter, block_for_inst(cfg, stmt.args[1]::Int))
        end
    elseif isa(stmt, GotoIfNot)
        stmt = GotoIfNot(stmt.cond, block_for_inst(cfg, stmt.dest))
    elseif stmt isa GotoNode
        stmt = GotoNode(block_for_inst(cfg, stmt.label))
    elseif stmt isa PhiNode
        e = stmt.edges
        stmt = PhiNode(Any[block_for_inst(cfg, e[i]) for i in 1:length(e)], stmt.values)
    end
    show_type = types isa Vector{Any} && should_print_ssa_type(stmt)
    print_stmt(io, idx, stmt, used, maxlength_idx, true, show_type)
    if types isa Vector{Any} # ignore types for pre-inference code
        if !isassigned(types, idx)
            # This is an error, but can happen if passes don't update their type information
            printstyled(io, "::#UNDEF", color=:red)
        elseif show_type
            typ = types[idx]
            line_info_postprinter(io, typ, idx in used)
        end
    end
    println(io)
    return bb_idx
end

function show_ir(io::IO, code::CodeInfo, line_info_preprinter=DILineInfoPrinter(code.linetable), line_info_postprinter=default_expr_type_printer)
    ioctx = IOContext(io, :displaysize => displaysize(io))
    stmts = code.code
    used = BitSet()
    cfg = compute_basic_blocks(stmts)
    for stmt in stmts
        scan_ssa_use!(push!, used, stmt)
    end
    bb_idx = 1

    for idx in 1:length(stmts)
        bb_idx = show_ir_stmt(ioctx, code, idx, line_info_preprinter, line_info_postprinter, used, cfg, bb_idx)
    end

    max_bb_idx_size = length(string(length(cfg.blocks)))
    line_info_preprinter(io, " "^(max_bb_idx_size + 2), typemin(Int32))
    nothing
end

@specialize
