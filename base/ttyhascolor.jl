# This file is a part of Julia. License is MIT: https://julialang.org/license

if Sys.iswindows()
    ttyhascolor(term_type = nothing) = true
else
    function ttyhascolor(term_type = get(ENV, "TERM", ""))
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
end

"""
    ttyhastruecolor()

Return a boolean signifying whether the current terminal supports 24-bit colors.

This uses the `COLORTERM` environment variable if possible, returning true if it
is set to either `"truecolor"` or `"24bit"`.

As a fallback, first on unix systems the `colors` terminal capability is checked
— should more than 256 colors be reported, this is taken to signify 24-bit
support. Lastly, the color is attempted to be set to `#010203` and then the
current color queried via the DCS (Device Control String) sequence `\$qm`. If
the output contains `":1:2:3"` this is taken to signify 24-bit support.

If the fallbacks are used, the `"COLORTERM"` entry in `ENV` is updated according
to the result. This ensures that frequent calls will only potentially be slow
the first time.
"""
function ttyhastruecolor()
    function test24bitcolor_dcs()
        REPL.Terminals.raw!(REPL.TerminalMenus.terminal, true)
        print(stdout, "\e[48;2;1;2;3m\eP\$qm\e\\\e[m")
        flush(stdout)
        # Some terminals are bad and haven't got DCS sequence support,
        # if we don't see a response from stdin we need to abort.
        output = @task readuntil(stdin, 'm')
        schedule(output)
        Timer(0.1) do _
            istaskdone(output) || Base.throwto(output, InterruptException())
        end
        color = try
            fetch(output)
        catch _ "" end
        REPL.Terminals.raw!(REPL.TerminalMenus.terminal, false)
        occursin(":1:2:3", color)
    end
    get(ENV, "COLORTERM", "") ∈ ("truecolor", "24bit") ||
        @static if Sys.isunix()
            get(current_terminfo, :colors, 0) > 256 ||
                isinteractive() && test24bitcolor_dcs()
        else
            false
        end
end

function get_have_color()
    global have_color
    have_color === nothing && (have_color = ttyhascolor())
    return have_color::Bool
end

function get_have_truecolor()
    global have_truecolor
    have_truecolor === nothing && (have_truecolor = ttyhastruecolor())
    return have_truecolor::Bool
end

in(key_value::Pair{Symbol,Bool}, ::TTY) = key_value.first === :color && key_value.second === get_have_color()
haskey(::TTY, key::Symbol) = key === :color
getindex(::TTY, key::Symbol) = key === :color ? get_have_color() : throw(KeyError(key))
get(::TTY, key::Symbol, default) = key === :color ? get_have_color() : default
