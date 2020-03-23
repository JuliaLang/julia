function ttyhascolor()
    term_type = get(ENV, "TERM","")
    startswith(term_type, "xterm") && return true
    try
        @static if Sys.KERNEL === :FreeBSD
            return success(`tput AF 0`)
        else
            return success(`tput setaf 0`)
        end
    catch e
        return false
    end
end
function get_have_color()
    global have_color
    have_color === nothing && (have_color = ttyhascolor())
    return have_color
end
in(key_value::Pair{Symbol,Bool}, ::TTY) = key_value.first === :color && key_value.second === get_have_color()
haskey(::TTY, key::Symbol) = key === :color
getindex(::TTY, key::Symbol) = key === :color ? get_have_color() : throw(KeyError(key))
get(::TTY, key::Symbol, default) = key === :color ? get_have_color() : default


