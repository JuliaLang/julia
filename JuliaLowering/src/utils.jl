# Error handling

TODO(msg::AbstractString) = throw(ErrorException("Lowering TODO: $msg"))
TODO(ex::SyntaxTree, msg="") = throw(LoweringError(ex, "Lowering TODO: $msg"))

# Errors found during lowering will result in LoweringError being thrown to
# indicate the syntax causing the error.
struct LoweringError <: Exception
    ex::SyntaxTree
    msg::String
end

function Base.showerror(io::IO, exc::LoweringError; show_detail=true)
    print(io, "LoweringError:\n")
    src = sourceref(exc.ex)
    highlight(io, src; note=exc.msg)

    if show_detail
        print(io, "\n\nDetailed provenance:\n")
        showprov(io, exc.ex, tree=true)
    end
end

#-------------------------------------------------------------------------------
function _show_provtree(io::IO, ex::SyntaxTree, indent)
    print(io, ex, "\n")
    prov = provenance(ex)
    for (i, e) in enumerate(prov)
        islast = i == length(prov)
        printstyled(io, "$indent$(islast ? "└─ " : "├─ ")", color=:light_black)
        inner_indent = indent * (islast ? "   " : "│  ")
        _show_provtree(io, e, inner_indent)
    end
end

function _show_provtree(io::IO, prov, indent)
    fn = filename(prov)
    line, _ = source_location(prov)
    printstyled(io, "@ $fn:$line\n", color=:light_black)
end

function showprov(io::IO, exs::AbstractVector;
                  note=nothing, include_location::Bool=true, highlight_kwargs...)
    for (i,ex) in enumerate(Iterators.reverse(exs))
        sr = sourceref(ex)
        if i > 1
            print(io, "\n\n")
        end
        k = kind(ex)
        if isnothing(note) # use provided `note` otherwise
            note = i > 1 && k == K"macrocall"  ? "in macro expansion" :
                   i > 1 && k == K"$"          ? "interpolated here"  :
                   "in source"
        end
        highlight(io, sr; note=note, highlight_kwargs...)

        if include_location
            line, _ = source_location(sr)
            locstr = "$(filename(sr)):$line"
            JuliaSyntax._printstyled(io, "\n# @ $locstr", fgcolor=:light_black)
        end
    end
end

function showprov(io::IO, ex::SyntaxTree; tree::Bool=false, showprov_kwargs...)
    if tree
        _show_provtree(io, ex, "")
    else
        showprov(io, flattened_provenance(ex); showprov_kwargs...)
    end
end

function showprov(x; kws...)
    showprov(stdout, x; kws...)
end

function subscript_str(i)
     replace(string(i),
             "0"=>"₀", "1"=>"₁", "2"=>"₂", "3"=>"₃", "4"=>"₄",
             "5"=>"₅", "6"=>"₆", "7"=>"₇", "8"=>"₈", "9"=>"₉")
end

function _deref_ssa(stmts, ex)
    while kind(ex) == K"SSAValue"
        ex = stmts[ex.var_id]
    end
    ex
end

function _find_method_lambda(ex, name)
    @assert kind(ex) == K"code_info"
    # Heuristic search through outer thunk for the method in question.
    method_found = false
    stmts = children(ex[1])
    for e in stmts
        if kind(e) == K"method" && numchildren(e) >= 2
            sig = _deref_ssa(stmts, e[2])
            @assert kind(sig) == K"call"
            arg_types = _deref_ssa(stmts, sig[2])
            @assert kind(arg_types) == K"call"
            self_type = _deref_ssa(stmts, arg_types[2])
            if kind(self_type) == K"globalref" && occursin(name, self_type.name_val)
                return e[3]
            end
        end
    end
end

function print_ir(io::IO, ex, method_filter=nothing)
    @assert kind(ex) == K"code_info"
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

function _print_ir(io::IO, ex, indent)
    added_indent = "    "
    @assert (kind(ex) == K"lambda" || kind(ex) == K"code_info") && kind(ex[1]) == K"block"
    if !ex.is_toplevel_thunk && kind(ex) == K"code_info"
        slots = ex.slots
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
    stmts = children(ex[1])
    for (i, e) in enumerate(stmts)
        lno = rpad(i, 3)
        if kind(e) == K"method" && numchildren(e) == 3
            print(io, indent, lno, " --- method ", string(e[1]), " ", string(e[2]))
            if kind(e[3]) == K"lambda" || kind(e[3]) == K"code_info"
                println(io)
                _print_ir(io, e[3], indent*added_indent)
            else
                println(io, " ", string(e[3]))
            end
        elseif kind(e) == K"opaque_closure_method"
            @assert numchildren(e) == 5
            print(io, indent, lno, " --- opaque_closure_method ")
            for i=1:4
                print(io, " ", e[i])
            end
            println(io)
            _print_ir(io, e[5], indent*added_indent)
        elseif kind(e) == K"code_info"
            println(io, indent, lno, " --- ", e.is_toplevel_thunk ? "thunk" : "code_info")
            _print_ir(io, e, indent*added_indent)
        else
            code = string(e)
            println(io, indent, lno, " ", code)
        end
    end
end
