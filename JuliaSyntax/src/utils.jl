
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


#-------------------------------------------------------------------------------
# Copy of the Meta.parse() API, but ensuring that we call the flisp parser
# rather than using Meta.parse() which may be using the JuliaSyntax parser.

"""
Like Meta.parse() but always call the flisp reference parser.
"""
function fl_parse(str::AbstractString; raise::Bool=true, depwarn::Bool=true)
    ex, pos = fl_parse(str, 1, greedy=true, raise=raise, depwarn=depwarn)
    if isa(ex,Expr) && ex.head === :error
        return ex
    end
    if pos <= ncodeunits(str)
        raise && throw(Meta.ParseError("extra token after end of expression"))
        return Expr(:error, "extra token after end of expression")
    end
    return ex
end

function fl_parse(str::AbstractString, pos::Integer; greedy::Bool=true, raise::Bool=true,
                  depwarn::Bool=true)
    ex, pos = _fl_parse_string(str, "none", 1, pos, greedy ? :statement : :atom)
    if raise && isa(ex,Expr) && ex.head === :error
        throw(Meta.ParseError(ex.args[1]))
    end
    return ex, pos
end

"""
Like Meta.parseall() but always call the flisp reference parser.
"""
function fl_parseall(text::AbstractString; filename="none", lineno=1)
    ex,_ = _fl_parse_string(text, String(filename), lineno, 1, :all)
    return ex
end

function _fl_parse_string(text::AbstractString, filename::AbstractString,
                          lineno::Integer, index::Integer, options)
    if index < 1 || index > ncodeunits(text) + 1
        throw(BoundsError(text, index))
    end
    ex, offset::Int = _fl_parse_hook(text, filename, lineno, index-1, options)
    ex, offset+1
end

