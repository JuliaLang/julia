# This file is a part of Julia. License is MIT: https://julialang.org/license

module Terminals

export
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

## TextTerminal ##

abstract type TextTerminal <: Base.AbstractPipe end

# INTERFACE
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
    out_stream::Base.IO
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
cmove_col(t::UnixTerminal, n) = (write(t.out_stream, '\r'); n > 1 && cmove_right(t, n - 1))

if is_windows()
    function raw!(t::TTYTerminal,raw::Bool)
        check_open(t.in_stream)
        if Base.ispty(t.in_stream)
            run(if raw
                    `stty raw -echo onlcr -ocrnl opost`
                else
                    `stty sane`
                end,t.in_stream,t.out_stream,t.err_stream)
            true
        else
            ccall(:jl_tty_set_mode,
                 Int32, (Ptr{Void},Int32),
                 t.in_stream.handle, raw) != -1
        end
    end
else
    function raw!(t::TTYTerminal, raw::Bool)
        check_open(t.in_stream)
        ccall(:jl_tty_set_mode, Int32, (Ptr{Void},Int32), t.in_stream.handle, raw) != -1
    end
end

# eval some of these definitions to insert CSI as a constant string
@eval enable_bracketed_paste(t::UnixTerminal) = write(t.out_stream, $"$(CSI)?2004h")
@eval disable_bracketed_paste(t::UnixTerminal) = write(t.out_stream, $"$(CSI)?2004l")
@eval end_keypad_transmit_mode(t::UnixTerminal) = # tput rmkx
    write(t.out_stream, $"$(CSI)?1l\x1b>")

@eval clear(t::UnixTerminal) = write(t.out_stream, $"$(CSI)H$(CSI)2J")
@eval clear_line(t::UnixTerminal) = write(t.out_stream, $"\r$(CSI)0K")
#beep(t::UnixTerminal) = write(t.err_stream,"\x7")

function Base.displaysize(t::UnixTerminal)
    return displaysize(t.out_stream)
end

if is_windows()
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

end # module
