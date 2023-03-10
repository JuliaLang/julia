# Compatibility hacks for older Julia versions
if VERSION < v"1.1"
    isnothing(x) = x === nothing
end
if VERSION < v"1.4"
    function only(x::AbstractVector)
        if length(x) != 1
            error("Collection must contain exactly 1 element")
        end
        return x[1]
    end
end
if VERSION < v"1.5"
    import Base.peek
end

#--------------------------------------------------
#
# Internal error, used as assertion failure for cases we expect can't happen.
@noinline function internal_error(strs...)
    error("Internal error: ", strs...)
end

# Like @assert, but always enabled and calls internal_error()
macro check(ex, msgs...)
    msg = isempty(msgs) ? ex : msgs[1]
    if isa(msg, AbstractString)
        msg = msg
    elseif !isempty(msgs) && (isa(msg, Expr) || isa(msg, Symbol))
        msg = :(string($(esc(msg))))
    else
        msg = string(msg)
    end
    return :($(esc(ex)) ? nothing : internal_error($msg))
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
# Text printing/display utils

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

