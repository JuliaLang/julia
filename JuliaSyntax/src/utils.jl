
"""
    Like printstyled, but allows providing RGB colors for true color terminals
"""
function _printstyled(io::IO, text; color)
    if length(color) != 3 || !all(0 .<= color .< 256)
        error("Invalid ansi color $color")
    end
    colcode = "\e[48;2;$(color[1]);$(color[2]);$(color[3])m"
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

