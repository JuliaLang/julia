# This file is a part of Julia. License is MIT: https://julialang.org/license

module Terminals

export
    AbstractTerminal,
    TextTerminal,
    UnixTerminal,
    TerminalBuffer,
    TTYTerminal,
    cmove,
    cmove_col,
    cmove_down,
    cmove_left,
    cmove_line_down,
    cmove_line_up,
    cmove_right,
    cmove_up,
    disable_bracketed_paste,
    enable_bracketed_paste,
    end_keypad_transmit_mode,
    getX,
    getY,
    hascolor,
    has_light_background,
    pos,
    raw!

import Base:
    check_open, # stream.jl
    displaysize,
    flush,
    pipe_reader,
    pipe_writer,
    read,
    readuntil

## AbstractTerminal: abstract supertype of all terminals ##

abstract type AbstractTerminal <: Base.AbstractPipe end

## TextTerminal ##

abstract type TextTerminal <: AbstractTerminal end

# Terminal interface:
pipe_reader(::TextTerminal) = error("Unimplemented")
pipe_writer(::TextTerminal) = error("Unimplemented")
displaysize(::TextTerminal) = error("Unimplemented")
cmove(t::TextTerminal, x, y) = error("Unimplemented")
getX(t::TextTerminal) = error("Unimplemented")
getY(t::TextTerminal) = error("Unimplemented")
pos(t::TextTerminal) = (getX(t), getY(t))

# Relative moves (Absolute position fallbacks)
cmove_up(t::TextTerminal, n) = cmove(getX(t), max(1, getY(t)-n))
cmove_up(t) = cmove_up(t, 1)

cmove_down(t::TextTerminal, n) = cmove(getX(t), max(height(t), getY(t)+n))
cmove_down(t) = cmove_down(t, 1)

cmove_left(t::TextTerminal, n) = cmove(max(1, getX(t)-n), getY(t))
cmove_left(t) = cmove_left(t, 1)

cmove_right(t::TextTerminal, n) = cmove(max(width(t), getX(t)+n), getY(t))
cmove_right(t) = cmove_right(t, 1)

cmove_line_up(t::TextTerminal, n) = cmove(1, max(1, getY(t)-n))
cmove_line_up(t) = cmove_line_up(t, 1)

cmove_line_down(t::TextTerminal, n) = cmove(1, max(height(t), getY(t)+n))
cmove_line_down(t) = cmove_line_down(t, 1)

cmove_col(t::TextTerminal, c) = cmove(c, getY(t))

# Defaults
hascolor(::TextTerminal) = false
has_light_background(::TextTerminal) = false

# Utility Functions
width(t::TextTerminal) = displaysize(t)[2]
height(t::TextTerminal) = displaysize(t)[1]

# For terminals with buffers
flush(t::TextTerminal) = nothing

clear(t::TextTerminal) = error("Unimplemented")
clear_line(t::TextTerminal, row) = error("Unimplemented")
clear_line(t::TextTerminal) = error("Unimplemented")

raw!(t::TextTerminal, raw::Bool) = error("Unimplemented")

beep(t::TextTerminal) = nothing
enable_bracketed_paste(t::TextTerminal) = nothing
disable_bracketed_paste(t::TextTerminal) = nothing

## UnixTerminal ##

abstract type UnixTerminal <: TextTerminal end

pipe_reader(t::UnixTerminal) = t.in_stream
pipe_writer(t::UnixTerminal) = t.out_stream

mutable struct TerminalBuffer <: UnixTerminal
    out_stream::IO
end

mutable struct TTYTerminal <: UnixTerminal
    term_type::String
    in_stream::IO
    out_stream::IO
    err_stream::IO
end

const CSI = "\x1b["

cmove_up(t::UnixTerminal, n) = write(t.out_stream, "$(CSI)$(n)A")
cmove_down(t::UnixTerminal, n) = write(t.out_stream, "$(CSI)$(n)B")
cmove_right(t::UnixTerminal, n) = write(t.out_stream, "$(CSI)$(n)C")
cmove_left(t::UnixTerminal, n) = write(t.out_stream, "$(CSI)$(n)D")
cmove_line_up(t::UnixTerminal, n) = (cmove_up(t, n); cmove_col(t, 1))
cmove_line_down(t::UnixTerminal, n) = (cmove_down(t, n); cmove_col(t, 1))
cmove_col(t::UnixTerminal, n) = (write(t.out_stream, '\r'); n > 1 && cmove_right(t, n-1))

const is_precompiling = Ref(false)
if Sys.iswindows()
    function raw!(t::TTYTerminal,raw::Bool)
        is_precompiling[] && return true
        check_open(t.in_stream)
        if Base.ispty(t.in_stream)
            run((raw ? `stty raw -echo onlcr -ocrnl opost` : `stty sane`),
                t.in_stream, t.out_stream, t.err_stream)
            true
        else
            ccall(:jl_tty_set_mode, Int32, (Ptr{Cvoid},Int32), t.in_stream.handle, raw) != -1
        end
    end
else
    function raw!(t::TTYTerminal, raw::Bool)
        check_open(t.in_stream)
        ccall(:jl_tty_set_mode, Int32, (Ptr{Cvoid},Int32), t.in_stream.handle, raw) != -1
    end
end

# eval some of these definitions to insert CSI as a constant string
@eval enable_bracketed_paste(t::UnixTerminal) = write(t.out_stream, $"$(CSI)?2004h")
@eval disable_bracketed_paste(t::UnixTerminal) = write(t.out_stream, $"$(CSI)?2004l")
@eval end_keypad_transmit_mode(t::UnixTerminal) = # tput rmkx
    write(t.out_stream, $"$(CSI)?1l\x1b>")

@eval clear(t::UnixTerminal) = write(t.out_stream, $"$(CSI)H$(CSI)2J")
@eval clear_line(t::UnixTerminal) = write(t.out_stream, $"\r$(CSI)0K")
beep(t::UnixTerminal) = write(t.err_stream,"\x7")

Base.displaysize(t::UnixTerminal) = displaysize(t.out_stream)

if Sys.iswindows()
    hascolor(t::TTYTerminal) = true
else
    function hascolor(t::TTYTerminal)
        startswith(t.term_type, "xterm") && return true
        try
            @static if Sys.KERNEL == :FreeBSD
                return success(`tput AF 0`)
            else
                return success(`tput setaf 0`)
            end
        catch
            return false
        end
    end
end

function has_light_background(term::TTYTerminal)
    # Don't bother if there is no support for colors
    !hascolor(term) && return false

    # This function is called when the user did not explicitly choose
    # either "light" or "dark" text color via a command line option.
    # We try various ways to determine the terminal's background
    # color, and use the first valid result. This is inspired by vim,
    # which uses a very similar approach:
    #
    # 1. First check the environment variable COLORFGBG
    # 2. Use the OSC 11 escape sequence to query the terminal
    # 3. Make an educated guess from the terminal name

    # See
    # <https://unix.stackexchange.com/questions/245378/common-environment-variable-to-set-dark-or-light-terminal-background>
    # for how to detect the terminal background color

    # Check COLORFGBG environment variable, which is set by rxvt and
    # derivatives. This variable contains either two or three values
    # separated by semicolons; we want the last value in either case.
    # If this value is 0-6 or 8, our background is dark.
    colorfgbg = get(ENV, "COLORFGBG", "")
    sep = findlast(";", colorfgbg)
    if sep !== nothing
        # See e.g. <https://en.wikipedia.org/wiki/ANSI_escape_code>
        # for the 16 possible colors
        colorbg = tryparse(Int, colorfgbg[last(sep)+1:end])
        if colorbg !== nothing && colorbg >= 0 && colorbg <= 15
            return colorbg == 7 || colorbg >= 9
        end
    end

    # Use OSC 11 escape sequence
    BEL = "\a"                  # "\007"
    ESC = "\e"                  # "\033"
    OSC = "$(ESC)]"
    send = "$(OSC)11;?$(BEL)"
    reply = mktemp() do path, io
        # Shell script idea taken from
        # <https://stackoverflow.com/questions/2507337/is-there-a-way-to-determine-a-terminals-background-color>
        cmds = ["printf %s '$send'",
                "sleep 0.00000001", # xterm needs the sleep
                "read -r recv",
                "printf %s \"\$recv\" >'$path'"]
        cmd = join(cmds, "; ")

        oldstty = chomp(read(open(`stty -g`, "r", term.in_stream).out, String))
        try
            read(open(`stty raw -echo min 0 time 0`, "r", term.in_stream).out,
                 String)
            run(pipeline(`/bin/sh -c $cmd`;
                         stdin=term.in_stream, stdout=term.out_stream))
        finally
            read(open(pipeline(`stty $oldstty`; stderr=devnull),
                      "r", term.in_stream).out, String)
        end
        String(read(io))
    end
    # We expect a reply such as "$(OSC)11;rgb:00/00/00$(BEL)" or
    # "$(OSC)11;rgba:0000/0000/0000/0000$(BEL)" or similar
    if startswith(reply, "$(OSC)11;rgb")
        col = findfirst(":", reply)
        bel = findfirst(BEL, reply)
        if col !== nothing && bel !== nothing
            colors = reply[col.stop+1 : bel.start-1]
            rgb = Float64[]
            for str in split(colors, "/")
                val = tryparse(UInt, str, base=16)
                if val !== nothing
                    maxval = UInt(1) << (4 * length(str)) - UInt(1)
                    val /= maxval # normalize
                    push!(rgb, val)
                end
            end
            if length(rgb) >= 3
                R, G, B = rgb
                Y = 0.299 * R + 0.587 * G + 0.114 * B
                return Y >= 0.5
            end
        end
    end

    # Guess from terminal name.
    # This list of terminal names is taken from vim.
    if term.term_type == "linux" ||         # Linux console
        term.term_type == "screen.linux" || # Linux console with screen
        startswith(term.term_type, "cygwin") || # Cygwin shell
        startswith(term.term_type, "putty")     # Putty progrem
        return false
    else
        return true
    end
end

Base.in(key_value::Pair, t::TTYTerminal) = in(key_value, pipe_writer(t))
Base.haskey(t::TTYTerminal, key) = haskey(pipe_writer(t), key)
Base.getindex(t::TTYTerminal, key) = getindex(pipe_writer(t), key)
Base.get(t::TTYTerminal, key, default) = get(pipe_writer(t), key, default)

Base.peek(t::TTYTerminal) = Base.peek(t.in_stream)

end # module
