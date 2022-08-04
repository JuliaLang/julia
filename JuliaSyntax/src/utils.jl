
"""
    Like printstyled, but allows providing RGB colors for true color terminals
"""
function _printstyled(io::IO, text; fgcolor=nothing, bgcolor=nothing)
    colcode = ""
    if !isnothing(fgcolor)
        if length(fgcolor) != 3 || !all(0 .<= fgcolor .< 256)
            error("Invalid ansi color $fgcolor")
        end
        colcode *= "\e[38;2;$(fgcolor[1]);$(fgcolor[2]);$(fgcolor[3])m"
    end
    if !isnothing(bgcolor)
        if length(bgcolor) != 3 || !all(0 .<= bgcolor .< 256)
            error("Invalid ansi color $bgcolor")
        end
        colcode *= "\e[48;2;$(bgcolor[1]);$(bgcolor[2]);$(bgcolor[3])m"
    end
    colreset = "\e[0;0m"
    first = true
    for linepart in split(text, '\n')
        first || print(io, '\n')
        print(io, colcode, linepart, colreset)
        first = false
    end
end

function flisp_parse_all(code; filename="none")
    if VERSION >= v"1.6"
        Meta.parseall(code, filename=filename)
    else
        # This is approximate. It should work for well-formed code.
        Base.parse_input_line(code, filename=filename)
    end
end

# Really remove line numbers, even from Expr(:toplevel)
remove_linenums!(ex) = ex
function remove_linenums!(ex::Expr)
    if ex.head === :block || ex.head === :quote || ex.head === :toplevel
        filter!(ex.args) do x
            !(isa(x, Expr) && x.head === :line || isa(x, LineNumberNode))
        end
    end
    for subex in ex.args
        subex isa Expr && remove_linenums!(subex)
    end
    return ex
end

