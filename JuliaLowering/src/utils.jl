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

function showprov(io::IO, exs::Vector)
    for (i,ex) in enumerate(Iterators.reverse(exs))
        sr = sourceref(ex)
        if i > 1
            print(io, "\n\n")
        end
        k = kind(ex)
        note = i > 1 && k == K"macrocall"  ? "in macro expansion" :
               i > 1 && k == K"$"          ? "interpolated here"  :
               "in source"
        highlight(io, sr, note=note)

        line, _ = source_location(sr)
        locstr = "$(filename(sr)):$line"
        JuliaSyntax._printstyled(io, "\n# @ $locstr", fgcolor=:light_black)
    end
end

function showprov(io::IO, ex::SyntaxTree; tree=false)
    if tree
        _show_provtree(io, ex, "")
    else
        showprov(io, flattened_provenance(ex))
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

function print_ir(io::IO, ex, indent="")
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
            println(io, indent, lno, " --- method ", string(e[1]), " ", string(e[2]))
            @assert kind(e[3]) == K"lambda" || kind(e[3]) == K"code_info"
            print_ir(io, e[3], indent*added_indent)
        elseif kind(e) == K"code_info" && e.is_toplevel_thunk
            println(io, indent, lno, " --- thunk")
            print_ir(io, e, indent*added_indent)
        else
            code = string(e)
            println(io, indent, lno, " ", code)
        end
    end
end

