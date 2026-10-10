# Within JL, :placeholder is used for never-read identifiers, but this magic
# symbol is used in the IR (its write-only properties are enforced in codegen).
const UNUSED = "#unused#"
# Method printing does stupid stuff with slot names; ideally move to provenance
# instead of reverse-engineering flisp slot naming
const ERASE_SLOTNAME_PREFIX = "#arg#"

TODO(msg::AbstractString) = throw(ErrorException("Lowering TODO: $msg"))
TODO(ex::SyntaxTree, msg="") = throw(LoweringError(ex, "Lowering TODO: $msg"))

"""
An error with detailed printing containing one or more SyntaxTrees and one
message per tree.  If `!internal`, caused by bad user code in `syntax` (flisp:
`Expr(:error, msg)`).
"""
struct LoweringError <: Exception
    sts::Vector{SyntaxTree}
    msgs::Vector{String}
    internal::Bool
end

@noinline LoweringError(ex::SyntaxTree, msg::String) =
    LoweringError(SyntaxList(ex), String[msg], false)

function Base.showerror(io::IO, exc::LoweringError; show_detail=true)
    println(io, exc.internal ? "internal lowering bug:" : "LoweringError:")
    for i in eachindex(exc.sts)
        st = exc.sts[i]
        msg = exc.msgs[i]
        src = sourceref(st)
        if src isa LineNumberNode
            l_str = (src.file === nothing || src.file === :var"") ?
                "line " : "$(src.file):"
            println(io, " at $(l_str)$(src.line): $msg")
        else
            highlight(io, src; note=msg)
        end
        if exc.internal || src isa LineNumberNode
            print(io, "\nExpression:\n  ")
            show(io, MIME"text/x.sexpression"(), st)
            # TODO: no parents available here; need to place them in LoweringError
            parents = SyntaxList()
            isempty(parents) || print(io, "\nContaining expressions:")
            for p in parents
                print(io, "\n  ")
                show(io, MIME"text/x.sexpression"(), p)
            end
        end
        i !== lastindex(exc.sts) && print(io, "\n\n")
    end

    if (exc.internal) && !isempty(exc.sts)
        print(io, "\n\nDetailed provenance:\n  ")
        _show_provtree(io, exc.sts[1], "  ")
    end
end

function _show_provtree(io::IO, ex::SyntaxTree, indent)
    print(io, ex)
    if ex.jl_source !== nothing
        printstyled(io, " @$(ex.jl_source)", color=:light_black)
    end
    prov = provenance(ex)

    print(io, "\n")

    src = ex.source
    msrc = macro_prov(ex)
    printstyled(io, string(
        indent, msrc === nothing ? "└─ " : "├─ "); color=:light_black)
    if src isa SyntaxTree
        _show_provtree(io, src, string(indent, msrc === nothing ? "   " : "│  "))
    else
        @jl_assert ex.source isa Union{LineNumberNode, SourceRef} ex
        lno = first_linenode(ex)
        printstyled(io, "@ $(lno.file):$(lno.line)\n", color=:light_black)
    end
    if msrc isa SyntaxTree
        printstyled(io, string(indent, "└─ "); color=:light_black)
        _show_provtree(io, msrc, indent*"   ")
    end
end

function showprov(io::IO, exs::AbstractVector;
                  note=nothing, include_location::Bool=true, highlight_kwargs...)
    for (i,ex) in enumerate(Iterators.reverse(exs))
        sr = sourceref(ex)
        if i > 1
            print(io, "\n\n")
        end
        k = head(ex)
        ex_note = !isnothing(note) ? note :
            i > 1 && k == :macrocall  ? "in macro expansion" :
            i > 1 && k == :$          ? "interpolated here"  :
            "in source"
        highlight(io, sr; note=ex_note, highlight_kwargs...)

        if include_location
            line, _ = source_location(sr)
            locstr = "$(filename(ex)):$line"
            JuliaSyntax._printstyled(io, "\n# @ $locstr", fgcolor=:light_black)
        end
    end
end

function showprov(io::IO, ex::SyntaxTree; showprov_kwargs...)
    showprov(io, flattened_provenance(ex); showprov_kwargs...)
end

function subscript_str(i)
     replace(string(i),
             "0"=>"₀", "1"=>"₁", "2"=>"₂", "3"=>"₃", "4"=>"₄",
             "5"=>"₅", "6"=>"₆", "7"=>"₇", "8"=>"₈", "9"=>"₉")
end

function _deref_ssa(stmts, ex)
    while head(ex) == :ssavalue
        ex = stmts[syntax_id(ex)]
    end
    ex
end

function _is_define_method_call(e)
    head(e) == :call && numchildren(e) >= 1 &&
        head(e[1]) == :core && syntax_name(e[1]) == "define_method"
end

function _find_method_lambda(ex0, name)
    ex = head(ex0) === :thunk ? ex0[1] : ex0
    @jl_assert head(ex) == :code_info ex
    # Heuristic search through outer thunk for the method in question.
    stmts = children(ex[2])
    for e in stmts
        if _is_define_method_call(e) && numchildren(e) == 5
            # define_method(module, fname, sig, lam)
            sig = _deref_ssa(stmts, e[4])
            @jl_assert head(sig) == :call ex
            arg_types = _deref_ssa(stmts, sig[2])
            @jl_assert head(arg_types) == :call ex
            self_type = _deref_ssa(stmts, arg_types[2])
            if head(self_type) == :globalref && occursin(name, syntax_name(self_type))
                return e[5]
            end
        end
    end
end

function print_ir(io::IO, ex, method_filter=nothing)
    @jl_assert head(ex) == :code_info || head(ex) == :thunk ex
    if !isnothing(method_filter)
        filtered = _find_method_lambda(ex, method_filter)
        if isnothing(filtered)
            @warn "Method not found with method filter $method_filter"
        else
            ex = filtered
        end
    end
    _print_ir(io, ex, "")
end

# TODO: JuliaLowering-the-module should always print the same way, ignoring parent modules
function _print_ir(io::IO, ex0, indent)
    added_indent = "    "
    (ex, is_toplevel_thunk) = head(ex0) === :thunk ? (ex0[1],true) : (ex0,false)
    @jl_assert ((head(ex) == :lambda || head(ex) == :code_info)
                && head(ex[2]) == :block) ex
    if !is_toplevel_thunk && head(ex) == :code_info
        slots = ex[1].value
        print(io, indent, "slots: [")
        for (i,slot) in enumerate(slots)
            print(io, "slot$(subscript_str(i))/$(slot.name)")
            flags = String[]
            slot.is_nospecialize   && push!(flags, "nospecialize")
            !slot.is_read          && push!(flags, "!read")
            slot.is_single_assign  && push!(flags, "single_assign")
            slot.is_maybe_undef    && push!(flags, "maybe_undef")
            slot.is_called         && push!(flags, "called")
            if !isempty(flags)
                print(io, "($(join(flags, ",")))")
            end
            if i < length(slots)
                print(io, " ")
            end
        end
        println(io, "]")
    end
    stmts = children(ex[2])
    for (i, e) in enumerate(stmts)
        lno = rpad(i, 3)
        if _is_define_method_call(e) && numchildren(e) == 5
            # define_method(module, fname, sig, lam)
            print(io, indent, lno, " (call core.define_method ",
                  string(e[2]), " ", string(e[3]), " ", string(e[4]))
            if head(e[5]) == :lambda || head(e[5]) == :code_info
                println(io)
                print(io, indent, "    --- code_info")
                println(io)
                _print_ir(io, e[5], indent*added_indent)
            else
                println(io, " ", string(e[5]), ")")
            end
        elseif head(e) == :opaque_closure_method
            @jl_assert numchildren(e) == 5 e
            print(io, indent, lno, " --- opaque_closure_method ")
            for i=1:4
                print(io, " ", e[i])
            end
            println(io)
            _print_ir(io, e[5], indent*added_indent)
        elseif head(e) == :code_info
            println(io, indent, lno, " --- ", "code_info")
            _print_ir(io, e, indent*added_indent)
        else
            code = string(e)
            println(io, indent, lno, " ", code)
        end
    end
end

# Wrap a function body in Base.Compiler.@zone for profiling
if isdefined(Base.Compiler, Symbol("@zone")) && DEBUG
    macro fzone(str, f)
        @assert(f isa Expr && f.head === :function && length(f.args) === 2 && str isa String,
                "usage: @fzone name_string <function expression>")
        esc(Expr(:function, f.args[1],
                 # Use source of our caller, not of this macro.
                 Expr(:macrocall, :(Base.Compiler.var"@zone"), __source__, str, f.args[2])))
    end
else
    macro fzone(str, f)
        esc(f)
    end
end

function _flatten_blocks(st::SyntaxTree)
    if head(st) === :block
        out = SyntaxList()
        for c in children(st)
            append!(out, _flatten_blocks(c))
        end
        # special case: an empty final block has value nothing
        if (length(children(st)) > 0 && head(st[end]) === :block &&
            numchildren(st[end]) == 0)
            push!(out, @ast _ st[end] (::nothing))
        end
        return out
    elseif is_quoted(st)
        SyntaxList(st)
    else
        SyntaxList(mapchildren(flatten_blocks, st))
    end
end

# Splat the contents of any block in `st` whose parent is also a block
function flatten_blocks(st::SyntaxTree)
    if head(st) === :block
        @mknode(st; children=_flatten_blocks(st))
    elseif is_quoted(st)
        st
    else
        mapchildren(flatten_blocks, st)
    end
end

# Hack.  Used for assignment to variables with `decl`, since the type may change
# between assignments.  flisp: renumber-assigned-ssavalues
function renumber_assigned_ssavalues(ctx, st)
    ssamap = Dict{IdTag, IdTag}()
    _find_assigned_ssavars!(ctx, ssamap, st)
    isempty(ssamap) && return st
    _replace_binding_ids(ctx, ssamap, st)
end
function _find_assigned_ssavars!(ctx, ssamap, st)
    (is_leaf(st) || is_quoted(st)) && return
    if head(st) == :(=) && head(st[1]) == :bindingid
        b = get_binding(ctx, st[1])
        b.is_ssa || return
        ssamap[b.id] = syntax_id(ssavar(ctx, st[1], b.name))
    end
    foreach(e->_find_assigned_ssavars!(ctx, ssamap, e), children(st))
end
function _replace_binding_ids(ctx, ssamap, st)
    if head(st) == :bindingid
        id = get(ssamap, syntax_id(st), nothing)
        isnothing(id) ? st : newleaf(st, :bindingid, id)
    elseif is_leaf(st) || is_quoted(st)
        st
    else
        mapchildren(e->_replace_binding_ids(ctx, ssamap, e), st)
    end
end
