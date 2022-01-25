
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
function remove_linenums!(ex)
    ex = Base.remove_linenums!(ex)
    if Meta.isexpr(ex, :toplevel)
        filter!(x->!(x isa LineNumberNode), ex.args)
    end
    ex
end

