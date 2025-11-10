# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file does not participate in bootstrapping, but is included in the system image by
# being loaded from `base/show.jl`. Compiler.jl as the standard library will simply include
# this file in the context of `Compiler.IRShow`.

using Base, Core.IR

import Base: show
using Base: isexpr, prec_decl, show_unquoted, with_output_color
using .Compiler: ALWAYS_FALSE, ALWAYS_TRUE, argextype, BasicBlock, block_for_inst,
    CachedMethodTable, CFG, compute_basic_blocks, DebugInfoStream, Effects,
    EMPTY_SPTYPES, getdebugidx, IncrementalCompact, InferenceResult, InferenceState,
    InvalidIRError, IRCode, LimitedAccuracy, NativeInterpreter, scan_ssa_use!,
    singleton_type, sptypes_from_meth_instance, StmtRange, Timings, VarState, widenconst,
    get_ci_mi, get_ci_abi

@nospecialize

function Base.show(io::IO, cfg::CFG)
    print(io, "CFG with $(length(cfg.blocks)) blocks:")
    for (idx, block) in enumerate(cfg.blocks)
        print(io, "\n  bb ", idx)
        if block.stmts.start == block.stmts.stop
            print(io, " (stmt ", block.stmts.start, ")")
        else
            print(io, " (stmts ", block.stmts.start, ":", block.stmts.stop, ")")
        end
        if !isempty(block.succs)
            print(io, " → bb ")
            join(io, block.succs, ", ")
        end
    end
end

function maybe_argextype(
    @nospecialize(x),
    src::Union{IRCode,IncrementalCompact,CodeInfo},
    sptypes::Vector{VarState},
)
    return try
        argextype(x, src, sptypes)
    catch err
        !(err isa InvalidIRError) && rethrow()
        nothing
    end
end

const inlined_apply_iterate_types = Union{Array,Memory,Tuple,NamedTuple,Core.SimpleVector}

function builtin_call_has_dispatch(
    @nospecialize(f),
    args::Vector{Any},
    src::Union{IRCode,IncrementalCompact,CodeInfo},
    sptypes::Vector{VarState},
)
    if f === Core._apply_iterate && length(args) >= 3
        # The implementation of _apply_iterate has hand-inlined implementations
        # for <builtin>(v::Union{Tuple,NamedTuple,Memory,Array,SimpleVector}...)
        # which perform no dynamic dispatch
        constructort = maybe_argextype(args[3], src, sptypes)
        if constructort === nothing || !(widenconst(constructort) <: Core.Builtin)
            return true
        end
        for arg in args[4:end]
            argt = maybe_argextype(arg, src, sptypes)
            if argt === nothing || !(widenconst(argt) <: inlined_apply_iterate_types)
                return true
            end
        end
    elseif (f === Core.invoke_in_world || f === Core._call_in_world_total || f === Core.invokelatest)
        # These apply-like builtins are effectively dynamic calls
        return true
    end
    return false
end

function print_stmt(io::IO, idx::Int, @nospecialize(stmt), code::Union{IRCode,CodeInfo,IncrementalCompact},
                    sptypes::Vector{VarState}, used::BitSet, maxlength_idx::Int, color::Bool, show_type::Bool, label_dynamic_calls::Bool)
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
    elseif isexpr(stmt, :invoke) && length(stmt.args) >= 2 && isa(stmt.args[1], Union{MethodInstance,CodeInstance})
        stmt = stmt::Expr
        # TODO: why is this here, and not in Base.show_unquoted
        ci = stmt.args[1]
        if ci isa Core.CodeInstance
            printstyled(io, "   invoke "; color = :light_black)
            mi = get_ci_mi(ci)
            abi = get_ci_abi(ci)
        else
            printstyled(io, "dynamic invoke "; color = :yellow)
            abi = (ci::Core.MethodInstance).specTypes
        end
        # XXX: this is wrong if `sig` is not a concretetype method
        # more correct would be to use `fieldtype(sig, i)`, but that would obscure / discard Varargs information in show
        sig = abi == Tuple ? Core.svec() : Base.unwrap_unionall(abi).parameters::Core.SimpleVector
        f = stmt.args[2]
        ft = maybe_argextype(f, code, sptypes)

        # We can elide the type for arg0 if it...
        skip_ftype = (length(sig) == 0) # doesn't exist...
        skip_ftype = skip_ftype || (
            # ... or, f prints as a user-accessible value...
            (f isa GlobalRef) &&
            # ... and matches the value of the singleton type of the invoked MethodInstance
            (singleton_type(ft) === singleton_type(sig[1]) !== nothing)
        )
        if skip_ftype
            show_unquoted(io, f, indent)
        else
            print(io, "(")
            show_unquoted(io, f, indent)
            print(io, "::", sig[1], ")")
        end

        # Print the remaining arguments (with type annotations from the invoked MethodInstance)
        print(io, "(")
        print_arg(i) = sprint(; context=io) do io
            show_unquoted(io, stmt.args[i], indent)
            if (i - 1) <= length(sig)
                print(io, "::", sig[i - 1])
            end
        end
        join(io, (print_arg(i) for i = 3:length(stmt.args)), ", ")
        print(io, ")")
        # TODO: if we have a CodeInstance, should we print that rettype info here, which may differ (wider or narrower than the ssavaluetypes)
    elseif isexpr(stmt, :call) && length(stmt.args) >= 1 && label_dynamic_calls
        ft = maybe_argextype(stmt.args[1], code, sptypes)
        f = singleton_type(ft)
        if isa(f, Core.IntrinsicFunction)
            printstyled(io, "intrinsic "; color = :light_black)
        elseif isa(f, Core.Builtin)
            if builtin_call_has_dispatch(f, stmt.args, code, sptypes)
                printstyled(io, "dynamic builtin "; color = :yellow)
            else
                printstyled(io, "  builtin "; color = :light_black)
            end
        elseif ft === nothing
            # This should only happen when, e.g., printing a call that targets
            # an out-of-bounds SSAValue or similar
            # (i.e. under normal circumstances, dead code)
            printstyled(io, "  unknown "; color = :light_black)
        elseif widenconst(ft) <: Core.Builtin
            printstyled(io, "dynamic builtin "; color = :yellow)
        else
            printstyled(io, "  dynamic "; color = :yellow)
        end
        show_unquoted(io, stmt, indent, show_type ? prec_decl : 0)
    # given control flow information, we prefer to print these with the basic block #, instead of the ssa %
    elseif isa(stmt, EnterNode)
        print(io, "enter #", stmt.catch_dest, "")
        if isdefined(stmt, :scope)
            print(io, " with scope ")
            show_unquoted(io, stmt.scope, indent)
        end
    elseif stmt isa GotoNode
        print(io, "goto #", stmt.label)
    elseif stmt isa PhiNode
        Base.show_unquoted_phinode(io, stmt, indent, "#")
    elseif stmt isa GotoIfNot
        Base.show_unquoted_gotoifnot(io, stmt, indent, "#")
    # everything else in the IR, defer to the generic AST printer
    else
        show_unquoted(io, stmt, indent, show_type ? prec_decl : 0)
    end
    nothing
end

function should_print_ssa_type(@nospecialize node)
    if isa(node, Expr)
        return !(node.head in (:gc_preserve_begin, :gc_preserve_end, :meta, :leave))
    end
    return !isa(node, PiNode)   && !isa(node, GotoIfNot) &&
           !isa(node, GotoNode) && !isa(node, ReturnNode) &&
           !isa(node, QuoteNode) && !isa(node, EnterNode)
end

function default_expr_type_printer(io::IO; @nospecialize(type), used::Bool, show_type::Bool=true, _...)
    show_type || return nothing
    printstyled(io, "::", type, color=(used ? :cyan : :light_black))
    return nothing
end

function method_name(@nospecialize m)
    if m isa LineInfoNode
        m = m.method
    end
    if m isa MethodInstance
        m = m.def
    end
    if m isa Method
        m = m.name
    end
    if m isa Module
        return :var"top-level scope"
    end
    if m isa Symbol
        return m
    end
    return :var""
end
@noinline function normalize_method_name(@nospecialize m)
    name = method_name(m)
    return name === :var"" ? :none : name
end

"""
    Compute line number annotations for an IRCode or CodeInfo.

This functions compute three sets of annotations for each IR line. Take the following
example (taken from `@code_typed sin(1.0)`):

```
    **                                                    ***         **********
    35 6 ── %10  = :(Base.mul_float)(%%2, %%2)::Float64   │╻╷         sin_kernel
       │    %11  = :(Base.mul_float)(%10, %10)::Float64   ││╻          *
```

The three annotations are indicated with `*`. The first one is the line number of the
active function (printed once whenever the outer most line number changes). The second
is the inlining indicator. The number of lines indicate the level of nesting, with a
half-size line (╷) indicating the start of a scope and a full size line (│) indicating
a continuing scope. The last annotation is the most complicated one. It is a heuristic
way to print the name of the entered scope. What it attempts to do is print the outermost
scope that hasn't been printed before. Let's work a number of examples to see the impacts
and tradeoffs involved.

```
f() = leaf_function() # Deliberately not defined to end up in the IR verbatim
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

Even though we were in the `f` scope since the first statement, it took us two statements
to catch up and print the intermediate scopes. Which scope is printed is indicated both
by the indentation of the method name and by an increased thickness of the appropriate
line for the scope.
"""
function compute_ir_line_annotations(code::Union{IRCode,CodeInfo})
    loc_annotations = String[]
    loc_methods = String[]
    loc_lineno = String[]
    cur_group = 1
    last_lineno = 0
    last_stack = LineInfoNode[] # nb. only file, line, and method are populated in this
    last_printed_depth = 0
    debuginfo = code.debuginfo
    def = :var"unknown scope"
    n = isa(code, IRCode) ? length(code.stmts) : length(code.code)
    for idx in 1:n
        buf = IOBuffer()
        print(buf, "│")
        stack = buildLineInfoNode(debuginfo, def, idx)
        lineno::Int = 0
        loc_method = ""
        isempty(stack) && (stack = last_stack)
        if !isempty(stack)
            lineno = stack[1].line
            x = min(length(last_stack), length(stack))
            depth = length(stack) - 1
            # Compute the last depth that was in common
            first_mismatch = let last_stack=last_stack, stack=stack
                findfirst(i->last_stack[i] != stack[i], 1:x)
            end
            # If the first mismatch is the last stack frame, that might just
            # be a line number mismatch in inner most frame. Ignore those
            if length(last_stack) == length(stack) && first_mismatch == length(stack)
                last_entry, entry = last_stack[end], stack[end]
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
                loc_method = normalize_method_name(stack[printing_depth + 1])
            end
            loc_method = string(" "^printing_depth, loc_method)
            last_stack = stack
        end
        push!(loc_annotations, takestring!(buf))
        push!(loc_lineno, (lineno != 0 && lineno != last_lineno) ? string(lineno) : "")
        push!(loc_methods, loc_method)
        (lineno != 0) && (last_lineno = lineno)
        nothing
    end
    return (loc_annotations, loc_methods, loc_lineno)
end

Base.show(io::IO, code::Union{IRCode, IncrementalCompact}) = show_ir(io, code)

# A line_info_preprinter for disabling line info printing
lineinfo_disabled(io::IO, linestart::String, idx::Int) = ""

# utility function to extract the file name from a DebugInfo object
function debuginfo_file1(debuginfo::Union{DebugInfo,DebugInfoStream})
    def = debuginfo.def
    if def isa MethodInstance
        def = def.def
    end
    if def isa Method
        def = def.file
    end
    if def isa Symbol
        return def
    end
    return :var"<unknown>"
end

# utility function to extract the first line number and file of a block of code
function debuginfo_firstline(debuginfo::Union{DebugInfo,DebugInfoStream})
    linetable = debuginfo.linetable
    while linetable != nothing
        debuginfo = linetable
        linetable = debuginfo.linetable
    end
    codeloc = getdebugidx(debuginfo, 0)
    return debuginfo_file1(debuginfo), codeloc[1]
end

struct LineInfoNode
    method # ::Union{Method,MethodInstance,Symbol}
    file::Symbol
    line::Int32
end

# utility function for converting a debuginfo object a particular pc to list of LineInfoNodes representing the inlining info at that pc for function `def`
# which is either `nothing` (macro-expand), a module (top-level), a Method (unspecialized code) or a MethodInstance (specialized code)
# Returns `false` if the line info should not be updated with this info because this
# statement has no effect on the line numbers. The `scopes` will still be populated however
# with as much information as was available about the inlining at that statement.
function append_scopes!(scopes::Vector{LineInfoNode}, pc::Int, debuginfo, @nospecialize(def))
    doupdate = true
    while true
        debuginfo.def isa Symbol || (def = debuginfo.def)
        codeloc = getdebugidx(debuginfo, pc)
        line::Int = codeloc[1]
        inl_to::Int = codeloc[2]
        doupdate &= line != 0 || inl_to != 0 # disabled debug info--no update
        if debuginfo.linetable === nothing || pc <= 0 || line < 0
            line < 0 && (doupdate = false; line = 0) # broken debug info
            push!(scopes, LineInfoNode(def, debuginfo_file1(debuginfo), Int32(line)))
        else
            doupdate = append_scopes!(scopes, line, debuginfo.linetable::DebugInfo, def) && doupdate
        end
        inl_to == 0 && return doupdate
        def = :var"macro expansion"
        debuginfo = debuginfo.edges[inl_to]
        pc::Int = codeloc[3]
    end
end

# utility wrapper around `append_scopes!` that returns an empty list instead of false
# when there is no applicable line update
function buildLineInfoNode(debuginfo, @nospecialize(def), pc::Int)
    DI = LineInfoNode[]
    append_scopes!(DI, pc, debuginfo, def) || empty!(DI)
    return DI
end

# A default line_info_preprinter for printing accurate line number information
function DILineInfoPrinter(debuginfo, def, showtypes::Bool=false)
    context = LineInfoNode[]
    context_depth = Ref(0)
    indent(s::String) = s^(max(context_depth[], 1) - 1)
    function emit_lineinfo_update(io::IO, linestart::String, pc::Int)
        # internal configuration options:
        linecolor = :yellow
        collapse = showtypes ? false : true
        indent_all = true
        # convert pc to a vector
        if pc == 0
            # sentinel value: reset internal (and external) state
            pops = indent("└")
            if !isempty(pops)
                print(io, linestart)
                printstyled(io, pops; color=linecolor)
                println(io)
            end
            empty!(context)
            context_depth[] = 0
            return ""
        end
        DI = reverse!(buildLineInfoNode(debuginfo, def, pc))
        if !isempty(DI)
            # FOR DEBUGGING, or if you just like very excessive output:
            # this prints out the context in full for every statement
            #empty!(context)
            #context_depth[] = 0
            nframes = length(DI)
            nctx::Int = 0
            pop_skips = 0
            # compute the size of the matching prefix in the inlining information stack
            for i = 1:min(length(context), nframes)
                CtxLine = context[i]
                FrameLine = DI[nframes - i + 1]
                CtxLine === FrameLine || break
                nctx = i
            end
            update_line_only::Bool = false
            if collapse
                if nctx > 0
                    # check if we're adding more frames with the same method name,
                    # if so, drop all existing calls to it from the top of the context
                    # AND check if instead the context was previously printed that way
                    # but now has removed the recursive frames
                    let method = method_name(context[nctx]) # last matching frame
                        if (nctx < nframes && method_name(DI[nframes - nctx]) === method) ||
                           (nctx < length(context) && method_name(context[nctx + 1]) === method)
                            update_line_only = true
                            while nctx > 0 && method_name(context[nctx]) === method
                                nctx -= 1
                            end
                        end
                    end
                end
                # look at the first non-matching element to see if we are only changing the line number
                if !update_line_only && nctx < length(context) && nctx < nframes
                    let CtxLine = context[nctx + 1],
                        FrameLine = DI[nframes - nctx]
                        if method_name(CtxLine) === method_name(FrameLine)
                            update_line_only = true
                        end
                    end
                end
            elseif nctx < length(context) && nctx < nframes
                # look at the first non-matching element to see if we are only changing the line number
                let CtxLine = context[nctx + 1],
                    FrameLine = DI[nframes - nctx]
                    if CtxLine.file === FrameLine.file &&
                            method_name(CtxLine) === method_name(FrameLine)
                        update_line_only = true
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
                resize!(context, nctx)
                update_line_only && (npops -= 1)
                if npops > 0
                    context_depth[] -= npops
                    print(io, linestart)
                    printstyled(io, indent("│"), "└"^npops; color=linecolor)
                    println(io)
                end
            end
            # now print the new frames
            while nctx < nframes
                frame::LineInfoNode = DI[nframes - nctx]
                nctx += 1
                started::Bool = false
                if !update_line_only && showtypes && !isa(frame.method, Symbol) && nctx != 1
                    print(io, linestart)
                    with_output_color(linecolor, io) do io
                        print(io, indent("│"))
                        print(io, "┌ invoke ", frame.method)
                        println(io)
                    end
                    started = true
                end
                print(io, linestart)
                with_output_color(linecolor, io) do io
                    print(io, indent("│"))
                    push!(context, frame)
                    if update_line_only
                        update_line_only = false
                    else
                        context_depth[] += 1
                        nctx != 1 && print(io, started ? "│" : "┌")
                    end
                    print(io, " @ ", frame.file)
                    if frame.line != typemax(frame.line) && frame.line != 0
                        print(io, ":", frame.line)
                    end
                    print(io, " within `", method_name(frame), "`")
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
        context_depth[] <= 1 && return ""
        return sprint(io -> printstyled(io, indent("│"), color=linecolor), context=io)
    end
    return emit_lineinfo_update
end

"""
    IRShowConfig

- `line_info_preprinter(io::IO, indent::String, idx::Int)`` may print relevant info
  at the beginning of the line, and should at least print `indent`. It returns a
  string that will be printed after the final basic-block annotation.
- `line_info_postprinter(io::IO; type, used::Bool, show_type::Bool, idx::Int)` prints
  relevant information like type-annotation at the end of the statement
- `should_print_stmt(idx::Int) -> Bool`: whether the statement at index `idx` should be
  printed as part of the IR or not
- `bb_color`: color used for printing the basic block brackets on the left
- `label_dynamic_calls`: whether to label calls as dynamic / builtin / intrinsic
"""
struct IRShowConfig
    line_info_preprinter
    line_info_postprinter
    should_print_stmt
    bb_color::Symbol
    label_dynamic_calls::Bool

    IRShowConfig(
        line_info_preprinter,
        line_info_postprinter=default_expr_type_printer;
        should_print_stmt=Returns(true),
        bb_color::Symbol=:light_black,
        label_dynamic_calls=true
    ) = new(
        line_info_preprinter,
        line_info_postprinter,
        should_print_stmt,
        bb_color,
        label_dynamic_calls
    )
end

struct _UNDEF
    global const UNDEF = _UNDEF.instance
end

function _stmt(code::IRCode, idx::Int)
    stmts = code.stmts
    return isassigned(stmts.stmt, idx) ? stmts[idx][:stmt] : UNDEF
end
function _stmt(compact::IncrementalCompact, idx::Int)
    stmts = compact.result
    return isassigned(stmts.stmt, idx) ? stmts[idx][:stmt] : UNDEF
end
function _stmt(code::CodeInfo, idx::Int)
    code = code.code
    return isassigned(code, idx) ? code[idx] : UNDEF
end

function _type(code::IRCode, idx::Int)
    stmts = code.stmts
    return isassigned(stmts.type, idx) ? stmts[idx][:type] : UNDEF
end
function _type(compact::IncrementalCompact, idx::Int)
    stmts = compact.result
    return isassigned(stmts.type, idx) ? stmts[idx][:type] : UNDEF
end
function _type(code::CodeInfo, idx::Int)
    types = code.ssavaluetypes
    types isa Vector{Any} || return nothing
    return isassigned(types, idx) ? types[idx] : UNDEF
end

function statement_indices_to_labels(stmt, cfg::CFG)
    # convert statement index to labels, as expected by print_stmt
    if stmt isa EnterNode
        stmt = EnterNode(stmt, stmt.catch_dest == 0 ? 0 : block_for_inst(cfg, stmt.catch_dest))
    elseif isa(stmt, GotoIfNot)
        stmt = GotoIfNot(stmt.cond, block_for_inst(cfg, stmt.dest))
    elseif stmt isa GotoNode
        stmt = GotoNode(block_for_inst(cfg, stmt.label))
    elseif stmt isa PhiNode
        e = stmt.edges
        stmt = PhiNode(Int32[block_for_inst(cfg, Int(e[i])) for i in 1:length(e)], stmt.values)
    end
    return stmt
end

# Show a single statement, code.stmts[idx]/code.code[idx], in the context of the whole IRCode/CodeInfo.
# Returns the updated value of bb_idx.
# pop_new_node!(idx::Int; attach_after=false) -> (node_idx, new_node_inst, new_node_type)
#   may return a new node at the current index `idx`, which is printed before the statement
#   at index `idx`. This function is repeatedly called until it returns `nothing`.
#   to iterate nodes that are to be inserted after the statement, set `attach_after=true`.
function show_ir_stmt(io::IO, code::Union{IRCode, CodeInfo, IncrementalCompact}, idx::Int, config::IRShowConfig,
                      sptypes::Vector{VarState}, used::BitSet, cfg::CFG, bb_idx::Int; pop_new_node! = Returns(nothing), only_after::Bool=false)
    return show_ir_stmt(io, code, idx, config.line_info_preprinter, config.line_info_postprinter,
                        sptypes, used, cfg, bb_idx; pop_new_node!, only_after, config.bb_color, config.label_dynamic_calls)
end

function show_ir_stmt(io::IO, code::Union{IRCode, CodeInfo, IncrementalCompact}, idx::Int, line_info_preprinter, line_info_postprinter,
                      sptypes::Vector{VarState}, used::BitSet, cfg::CFG, bb_idx::Int; pop_new_node! = Returns(nothing), only_after::Bool=false,
                      bb_color=:light_black, label_dynamic_calls::Bool=true)
    stmt = _stmt(code, idx)
    type = _type(code, idx)
    max_bb_idx_size = length(string(length(cfg.blocks)))

    if isempty(used)
        maxlength_idx = 0
    else
        maxused = maximum(used)
        maxlength_idx = length(string(maxused))
    end

    if stmt === UNDEF
        # This is invalid, but do something useful rather
        # than erroring, to make debugging easier
        printstyled(io, "#UNDEF\n", color=:red)
        return bb_idx
    end

    i = 1
    function print_indentation(final::Bool=true)
        # Compute BB guard rail
        if bb_idx > length(cfg.blocks)
            # If invariants are violated, print a special leader
            linestart = " "^(max_bb_idx_size + 2) # not inside a basic block bracket
            inlining_indent = line_info_preprinter(io, linestart, i == 1 ? idx : 0)
            printstyled(io, "!!! ", "─"^max_bb_idx_size, color=bb_color)
        else
            bbrange = cfg.blocks[bb_idx].stmts
            # Print line info update
            linestart = idx == first(bbrange) ? "  " : sprint(io -> printstyled(io, "│ ", color=bb_color), context=io)
            linestart *= " "^max_bb_idx_size
            # idx == 0 means only indentation is printed, so we don't print linfos
            # multiple times if the are new nodes
            inlining_indent = line_info_preprinter(io, linestart, i == 1 ? idx : 0)

            if i == 1 && idx == first(bbrange)
                bb_idx_str = string(bb_idx)
                bb_pad = max_bb_idx_size - length(bb_idx_str)
                bb_type = length(cfg.blocks[bb_idx].preds) <= 1 ? "─" : "┄"
                printstyled(io, bb_idx_str, " ", bb_type, "─"^bb_pad, color=bb_color)
            elseif final && idx == last(bbrange) # print separator
                printstyled(io, "└", "─"^(1 + max_bb_idx_size), color=bb_color)
            else
                printstyled(io, "│ ", " "^max_bb_idx_size, color=bb_color)
            end
        end
        print(io, inlining_indent, " ")
    end

    # first, print new nodes that are to be inserted before the current statement
    function print_new_node(node; final::Bool=true)
        print_indentation(final)

        node_idx, new_node_inst, new_node_type = node
        @assert new_node_inst !== UNDEF # we filtered these out earlier
        show_type = should_print_ssa_type(new_node_inst)
        let maxlength_idx=maxlength_idx, show_type=show_type
            with_output_color(:green, io) do io′
                print_stmt(io′, node_idx, new_node_inst, code, sptypes, used, maxlength_idx, false, show_type, label_dynamic_calls)
            end
        end

        if new_node_type === UNDEF # try to be robust against errors
            printstyled(io, "::#UNDEF", color=:red)
        else
            line_info_postprinter(io; type = new_node_type, used = node_idx in used, show_type, idx = node_idx)
        end
        println(io)
    end
    while (next = pop_new_node!(idx)) !== nothing
        only_after || print_new_node(next; final=false)
        i += 1
    end

    # peek at the nodes to be inserted after the current statement
    # (to determine of the statement itself is the final one)
    next = pop_new_node!(idx; attach_after=true)

    # then, print the current statement
    # FIXME: `only_after` is hack so that we can call this function to print uncompacted
    #        attach-after nodes when the current node has already been compated already
    if !only_after
        print_indentation(next===nothing)
        if code isa CodeInfo
            stmt = statement_indices_to_labels(stmt, cfg)
        end
        show_type = type !== nothing && should_print_ssa_type(stmt)
        print_stmt(io, idx, stmt, code, sptypes, used, maxlength_idx, true, show_type, label_dynamic_calls)
        if type !== nothing # ignore types for pre-inference code
            if type === UNDEF
                # This is an error, but can happen if passes don't update their type information
                printstyled(io, "::#UNDEF", color=:red)
            else
                line_info_postprinter(io; type, used = idx in used, show_type, idx)
            end
        end
        println(io)
    end
    i += 1

    # finally, print new nodes that are to be inserted after the current statement
    while next !== nothing
        print_new_node(next)
        i += 1
        next = pop_new_node!(idx; attach_after=true)
    end

    # increment the basic block counter
    if bb_idx <= length(cfg.blocks)
        bbrange = cfg.blocks[bb_idx].stmts
        if bb_idx <= length(cfg.blocks) && idx == last(bbrange)
            bb_idx += 1
        end
    end

    return bb_idx
end

function _new_nodes_iter(stmts, new_nodes, new_nodes_info, new_nodes_idx)
    new_nodes_perm = filter(i -> isassigned(new_nodes.stmt, i), 1:length(new_nodes))
    sort!(new_nodes_perm, by = x -> (x = new_nodes_info[x]; (x.pos, x.attach_after)))

    # separate iterators for the nodes that are inserted before resp. after each statement
    before_iter = Ref(1)
    after_iter = Ref(1)

    return function get_new_node(idx::Int; attach_after=false)
        iter = attach_after ? after_iter : before_iter
        iter[] <= length(new_nodes_perm) || return nothing
        node_idx = new_nodes_perm[iter[]]

        # skip nodes
        while node_idx < new_nodes_idx ||                           # already compacted
              idx > new_nodes_info[node_idx].pos ||                 # not interested in
              new_nodes_info[node_idx].attach_after != attach_after
            iter[] += 1
            iter[] > length(new_nodes_perm) && return nothing
            node_idx = new_nodes_perm[iter[]]
        end

        if new_nodes_info[node_idx].pos != idx ||
           new_nodes_info[node_idx].attach_after != attach_after
            return nothing
        end

        iter[] += 1
        new_node = new_nodes[node_idx]
        new_node_inst = isassigned(new_nodes.stmt, node_idx) ? new_node[:stmt] : UNDEF
        new_node_type = isassigned(new_nodes.type, node_idx) ? new_node[:type] : UNDEF
        node_idx += length(stmts)
        return node_idx, new_node_inst, new_node_type
    end
end

function new_nodes_iter(ir::IRCode, new_nodes_idx=1)
    stmts = ir.stmts
    new_nodes = ir.new_nodes.stmts
    new_nodes_info = ir.new_nodes.info
    return _new_nodes_iter(stmts, new_nodes, new_nodes_info, new_nodes_idx)
end

function new_nodes_iter(compact::IncrementalCompact)
    stmts = compact.result
    new_nodes = compact.new_new_nodes.stmts
    new_nodes_info = compact.new_new_nodes.info
    return _new_nodes_iter(stmts, new_nodes, new_nodes_info, 1)
end

# print only line numbers on the left, some of the method names and nesting depth on the right
function inline_linfo_printer(code::Union{IRCode,CodeInfo})
    loc_annotations, loc_methods, loc_lineno = compute_ir_line_annotations(code)
    max_loc_width = maximum(length, loc_annotations)
    max_lineno_width = maximum(length, loc_lineno)
    max_method_width = maximum(length, loc_methods)

    function (io::IO, indent::String, idx::Int)
        cols = (displaysize(io)::Tuple{Int,Int})[2]

        if idx == 0
            annotation = ""
            loc_method = ""
            lineno = ""
        elseif idx <= length(loc_annotations)
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
        if get(io, :color, false)::Bool
            method_start_column = cols - max_method_width - max_loc_width - 2
            filler = " "^(max_loc_width-length(annotation))
            printstyled(io, "\e[$(method_start_column)G$(annotation)$(filler)$(loc_method)\e[1G", color = :light_black)
        end
        printstyled(io, lineno, " "^(max_lineno_width - length(lineno) + 1); color = :light_black)
        return ""
    end
end

_strip_color(s::String) = replace(s, r"\e\[\d+m"a => "")

statementidx_lineinfo_printer(f, code::IRCode) = f(code.debuginfo, :var"unknown scope")
statementidx_lineinfo_printer(f, code::CodeInfo) = f(code.debuginfo, :var"unknown scope")
statementidx_lineinfo_printer(code) = statementidx_lineinfo_printer(DILineInfoPrinter, code)

function stmts_used(io::IO, code::IRCode, warn_unset_entry=true)
    insts = code.stmts
    used = BitSet()
    for inst in insts
        scan_ssa_use!(push!, used, inst[:stmt])
    end
    new_nodes = code.new_nodes.stmts
    for nn in 1:length(new_nodes)
        if isassigned(new_nodes.stmt, nn)
            scan_ssa_use!(push!, used, new_nodes[nn][:stmt])
        elseif warn_unset_entry
            printstyled(io, "ERROR: New node array has unset entry\n", color=:red)
            warn_unset_entry = false
        end
    end
    return used
end

function stmts_used(::IO, code::CodeInfo)
    stmts = code.code
    used = BitSet()
    for stmt in stmts
        scan_ssa_use!(push!, used, stmt)
    end
    return used
end

function default_config(code::IRCode; debuginfo = :source_inline)
    return IRShowConfig(get_debuginfo_printer(code, debuginfo); bb_color=:normal)
end
default_config(code::CodeInfo; debuginfo = :source) = IRShowConfig(get_debuginfo_printer(code, debuginfo))
function default_config(io::IO, src)
    debuginfo = get(io, :debuginfo, nothing)
    debuginfo !== nothing && return default_config(src; debuginfo)
    return default_config(src)
end

function show_ir_stmts(io::IO, ir::Union{IRCode, CodeInfo, IncrementalCompact}, inds, config::IRShowConfig,
                       sptypes::Vector{VarState}, used::BitSet, cfg::CFG, bb_idx::Int; pop_new_node! = Returns(nothing))
    for idx in inds
        if config.should_print_stmt(ir, idx, used)
            bb_idx = show_ir_stmt(io, ir, idx, config, sptypes, used, cfg, bb_idx; pop_new_node!)
        elseif bb_idx <= length(cfg.blocks) && idx == cfg.blocks[bb_idx].stmts.stop
            bb_idx += 1
        end
    end
    return bb_idx
end

function finish_show_ir(io::IO, cfg::CFG, config::IRShowConfig)
    max_bb_idx_size = length(string(length(cfg.blocks)))
    config.line_info_preprinter(io, " "^(max_bb_idx_size + 2), 0)
    return nothing
end

function show_ir(io::IO, ir::IRCode, config::IRShowConfig=default_config(io, ir); pop_new_node! = new_nodes_iter(ir))
    used = stmts_used(io, ir)
    cfg = ir.cfg
    maxssaid = length(ir.stmts) + length(ir.new_nodes)
    let io = IOContext(io, :maxssaid=>maxssaid)
        show_ir_stmts(io, ir, 1:length(ir.stmts), config, ir.sptypes, used, cfg, 1; pop_new_node!)
    end
    finish_show_ir(io, cfg, config)
end

function show_ir(io::IO, ci::CodeInfo, config::IRShowConfig=default_config(io, ci);
                 pop_new_node! = Returns(nothing))
    used = stmts_used(io, ci)
    cfg = compute_basic_blocks(ci.code)
    parent = ci.parent
    sptypes = if parent isa MethodInstance
        sptypes_from_meth_instance(parent)
    else EMPTY_SPTYPES end
    let io = IOContext(io, :maxssaid=>length(ci.code))
        show_ir_stmts(io, ci, 1:length(ci.code), config, sptypes, used, cfg, 1; pop_new_node!)
    end
    finish_show_ir(io, cfg, config)
end

function show_ir(io::IO, compact::IncrementalCompact, config::IRShowConfig=default_config(io, compact.ir))
    cfg = compact.ir.cfg


    # First print everything that has already been compacted

    # merge uses in uncompacted region into compacted uses
    used_compacted = BitSet(i for (i, x) in pairs(compact.used_ssas) if x != 0)
    used_uncompacted = stmts_used(io, compact.ir)
    for (i, ssa) = enumerate(compact.ssa_rename)
        if isa(ssa, SSAValue) && ssa.id in used_uncompacted
            push!(used_compacted, i)
        end
    end

    # while compacting, the end of the active result bb will not have been determined
    # (this is done post-hoc by `finish_current_bb!`), so determine it here from scratch.
    result_bbs = copy(compact.cfg_transform.result_bbs)
    if compact.active_result_bb <= length(result_bbs)
        # count the total number of nodes we'll add to this block
        input_bb_idx = block_for_inst(compact.ir.cfg, compact.idx)
        input_bb = compact.ir.cfg.blocks[input_bb_idx]
        count = 0
        for input_idx in input_bb.stmts.start:input_bb.stmts.stop
            pop_new_node! = new_nodes_iter(compact.ir)
            while pop_new_node!(input_idx) !== nothing
                count += 1
            end
            while pop_new_node!(input_idx; attach_after=true) !== nothing
                count += 1
            end
        end

        still_to_be_inserted = (last(input_bb.stmts) - compact.idx) + count

        result_bb = result_bbs[compact.active_result_bb]
        result_bbs[compact.active_result_bb] = BasicBlock(result_bb,
            StmtRange(first(result_bb.stmts), compact.result_idx+still_to_be_inserted))
    end
    compact_cfg = CFG(result_bbs, Int[first(result_bbs[i].stmts) for i in 2:length(result_bbs)])

    pop_new_node! = new_nodes_iter(compact)
    maxssaid = length(compact.result) + length(compact.new_new_nodes)
    bb_idx = let io = IOContext(io, :maxssaid=>maxssaid)
        show_ir_stmts(io, compact, 1:compact.result_idx-1, config, compact.ir.sptypes,
                      used_compacted, compact_cfg, 1; pop_new_node!)
    end


    # Print uncompacted nodes from the original IR

    # print a separator
    (_, width) = displaysize(io)
    stmts = compact.ir.stmts
    indent = length(string(length(stmts)))
    # config.line_info_preprinter(io, "", compact.idx)
    printstyled(io, "─"^(width-indent-1), '\n', color=:red)

    # while compacting, the start of the active uncompacted bb will have been overwritten.
    # this manifests as a stmt range end that is less than the start, so correct that.
    inputs_bbs = copy(cfg.blocks)
    for (i, bb) in enumerate(inputs_bbs)
        if bb.stmts.stop < bb.stmts.start
            inputs_bbs[i] = BasicBlock(bb,
                StmtRange(last(bb.stmts), last(bb.stmts)))
            # this is not entirely correct, and will result in the bb starting again,
            # but is the best we can do without changing how `finish_current_bb!` works.
        end
    end
    uncompacted_cfg = CFG(inputs_bbs, Int[first(inputs_bbs[i].stmts) for i in 2:length(inputs_bbs)])

    pop_new_node! = new_nodes_iter(compact.ir, compact.new_nodes_idx)
    maxssaid = length(compact.ir.stmts) + length(compact.ir.new_nodes)
    let io = IOContext(io, :maxssaid=>maxssaid)
        # first show any new nodes to be attached after the last compacted statement
        if compact.idx > 1
            show_ir_stmt(io, compact.ir, compact.idx-1, config, compact.ir.sptypes,
                         used_uncompacted, uncompacted_cfg, bb_idx; pop_new_node!, only_after=true)
        end

        # then show the actual uncompacted IR
        show_ir_stmts(io, compact.ir, compact.idx:length(stmts), config, compact.ir.sptypes,
                      used_uncompacted, uncompacted_cfg, bb_idx; pop_new_node!)
    end

    finish_show_ir(io, uncompacted_cfg, config)
end

function effectbits_letter(effects::Effects, name::Symbol, suffix::Char)
    ft = fieldtype(Effects, name)
    if ft === UInt8
        prefix = getfield(effects, name) === ALWAYS_TRUE ? '+' :
                 getfield(effects, name) === ALWAYS_FALSE ? '!' : '?'
    elseif ft === Bool
        prefix = getfield(effects, name) ? '+' : '!'
    else
        error("unsupported effectbits type given")
    end
    return string(prefix, suffix)
end

function effectbits_color(effects::Effects, name::Symbol)
    ft = fieldtype(Effects, name)
    if ft === UInt8
        color = getfield(effects, name) === ALWAYS_TRUE ? :green :
                getfield(effects, name) === ALWAYS_FALSE ? :red : :yellow
    elseif ft === Bool
        color = getfield(effects, name) ? :green : :red
    else
        error("unsupported effectbits type given")
    end
    return color
end

function Base.show(io::IO, e::Effects)
    print(io, "(")
    printstyled(io, effectbits_letter(e, :consistent,  'c'); color=effectbits_color(e, :consistent))
    print(io, ',')
    printstyled(io, effectbits_letter(e, :effect_free, 'e'); color=effectbits_color(e, :effect_free))
    print(io, ',')
    printstyled(io, effectbits_letter(e, :nothrow,     'n'); color=effectbits_color(e, :nothrow))
    print(io, ',')
    printstyled(io, effectbits_letter(e, :terminates,  't'); color=effectbits_color(e, :terminates))
    print(io, ',')
    printstyled(io, effectbits_letter(e, :notaskstate, 's'); color=effectbits_color(e, :notaskstate))
    print(io, ',')
    printstyled(io, effectbits_letter(e, :inaccessiblememonly, 'm'); color=effectbits_color(e, :inaccessiblememonly))
    print(io, ',')
    printstyled(io, effectbits_letter(e, :noub, 'u'); color=effectbits_color(e, :noub))
    print(io, ',')
    printstyled(io, effectbits_letter(e, :nonoverlayed, 'o'); color=effectbits_color(e, :nonoverlayed))
    print(io, ',')
    printstyled(io, effectbits_letter(e, :nortcall, 'r'); color=effectbits_color(e, :nortcall))
    print(io, ')')
end

function Base.show(io::IO, inferred::InferenceResult)
    mi = inferred.linfo
    tt = mi.specTypes.parameters[2:end]
    tts = join(["::$(t)" for t in tt], ", ")
    rettype = inferred.result
    if isa(rettype, InferenceState)
        rettype = rettype.bestguess
    end
    if isa(mi.def, Method)
        print(io, mi.def.name, "(", tts, " => ", rettype, ")")
    else
        print(io, "Toplevel MethodInstance thunk from ", mi.def, " => ", rettype)
    end
end

Base.show(io::IO, sv::InferenceState) =
    (print(io, "InferenceState for "); show(io, sv.linfo))

Base.show(io::IO, ::NativeInterpreter) =
    print(io, "Compiler.NativeInterpreter(...)")

Base.show(io::IO, cache::CachedMethodTable) =
    print(io, typeof(cache), "(", length(cache.cache), " entries)")

function Base.show(io::IO, limited::LimitedAccuracy)
    print(io, "LimitedAccuracy(")
    show(io, limited.typ)
    print(io, ", #= ", length(limited.causes), " cause(s) =#)")
end

# These sometimes show up as Const-values in InferenceFrameInfo signatures
function Base.show(io::IO, mi_info::Timings.InferenceFrameInfo)
    mi = mi_info.mi
    def = mi.def
    if isa(def, Method)
        if isdefined(def, :generator) && mi === def.generator
            print(io, "InferenceFrameInfo generator for ")
            show(io, def)
        else
            print(io, "InferenceFrameInfo for ")
            argnames = [isa(a, Core.Const) ? (isa(a.val, Type) ? "" : a.val) : "" for a in mi_info.slottypes[1:mi_info.nargs]]
            show_tuple_as_call(io, def.name, mi.specTypes; argnames, qualified=true)
        end
    else
        di = mi.cache.debuginfo
        file, line = debuginfo_firstline(di)
        file = string(file)
        line = isempty(file) || line < 0 ? "<unknown>" : "$file:$line"
        print(io, "Toplevel InferenceFrameInfo thunk from ", def, " starting at ", line)
    end
end

function Base.show(io::IO, tinf::Timings.Timing)
    print(io, "Compiler.Timings.Timing(", tinf.mi_info, ") with ", length(tinf.children), " children")
end

@specialize

const __debuginfo = Dict{Symbol, Any}(
    # :full => src -> statementidx_lineinfo_printer(src), # and add variable slot information
    :source => src -> statementidx_lineinfo_printer(src),
    # :oneliner => src -> statementidx_lineinfo_printer(PartialLineInfoPrinter, src),
    :none => src -> lineinfo_disabled,
    )
const default_debuginfo = Ref{Symbol}(:none)
debuginfo(sym) = sym === :default ? default_debuginfo[] : sym

const __debuginfo = Dict{Symbol, Any}(
    # :full => src -> statementidx_lineinfo_printer(src), # and add variable slot information
    :source => src -> statementidx_lineinfo_printer(src),
    :source_inline => src -> inline_linfo_printer(src),
    # :oneliner => src -> statementidx_lineinfo_printer(PartialLineInfoPrinter, src),
    :none => src -> lineinfo_disabled,
    )

const debuginfo_modes = [:none, :source, :source_inline]
@assert Set(debuginfo_modes) == Set(keys(__debuginfo))

function validate_debuginfo_mode(mode::Symbol)
    in(mode, debuginfo_modes) && return true
    throw(ArgumentError("`debuginfo` must be one of the following: $(join([repr(mode) for mode in debuginfo_modes], ", "))"))
end

const default_debuginfo_mode = Ref{Symbol}(:none)
function expand_debuginfo_mode(mode::Symbol, default = default_debuginfo_mode[])
    if mode === :default
        mode = default
    end
    validate_debuginfo_mode(mode)
    return mode
end

function get_debuginfo_printer(mode::Symbol)
    mode = expand_debuginfo_mode(mode)
    return __debuginfo[mode]
end

get_debuginfo_printer(src, mode::Symbol) = get_debuginfo_printer(mode)(src)
