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
    csave,
    crestore,
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
    readuntil,
    active_repl

## AbstractTerminal: abstract supertype of all terminals ##

abstract type AbstractTerminal <: Base.AbstractPipe end

## TextTerminal ##

abstract type TextTerminal <: AbstractTerminal end

# Terminal interface:
pipe_reader(::TextTerminal=active_repl.t) = error("Unimplemented")
pipe_writer(::TextTerminal=active_repl.t) = error("Unimplemented")
displaysize(::TextTerminal=active_repl.t) = error("Unimplemented")
cmove(t::TextTerminal, x, y) = error("Unimplemented")
cmove(x, y) = cmove(active_repl.t, x, y)
"""
    csave()
    csave(t)

Save the cursor position in TextTerminal `t`. If
`t` is not provided, it defaults to `Base.active_repl.t`.

Note that if this is run in the REPL, the cursor moves as it is run,
and the final position is the one saved. This means that
the saved position is just after "julia>" on the next line.
"""
csave(t::TextTerminal=active_repl.t) = error("Unimplemented")
"""
    crestore()
    crestore(t)

Restore the the cursor position in TextTerminal `t`to the
position set by `csave`. If `t` is not provided, it defaults
to `Base.active_repl.t`.

Note that after the cursor position is restores, anything
to the right it on the same line is cleared.
"""
crestore(t::TextTerminal=active_repl.t) = error("Unimplemented")
getX(t::TextTerminal=active_repl.t) = error("Unimplemented")
getY(t::TextTerminal=active_repl.t) = error("Unimplemented")
pos(t::TextTerminal=active_repl.t) = (getX(t), getY(t))

# Absolute fallbacks are provided for relative movements
cmove_up(t::TextTerminal, n=1) = cmove(getX(t), max(1, getY(t) - n))
cmove_up(n=1) = cmove_up(Base.current_repl.t, n)

cmove_down(t::TextTerminal, n=1) = cmove(getX(t), max(height(t), getY(t) + n))
cmove_down(n=1) = cmove_down(Base.current_repl.t, n)

cmove_left(t::TextTerminal, n=1) = cmove(max(1, getX(t) - n), getY(t))
cmove_left(n=1) = cmove_left(Base.current_repl.t, n)

cmove_right(t::TextTerminal, n=1) = cmove(max(width(t), getX(t) + n), getY(t))
cmove_right(n=1) = cmove_right(Base.current_repl.t, n)

cmove_line_up(t::TextTerminal, n=1) = cmove(1, max(1, getY(t) - n))
cmove_line_up(n=1) = cmove_line_up(Base.current_repl.t, n)

cmove_line_down(t::TextTerminal, n=1) = cmove(1, max(height(t), getY(t) + n))
cmove_line_down(n=1) = cmove_line_down(Base.current_repl.t, n)

cmove_col(t::TextTerminal, c) = cmove(c, getY(t))
cmove_col(c) = cmove_col(Base.current_repl.t, n)

# Defaults
hascolor(::TextTerminal=active_repl.t) = false

# Utility Functions
width(t::TextTerminal=active_repl.t) = (displaysize(t)::Tuple{Int,Int})[2]
height(t::TextTerminal=active_repl.t) = (displaysize(t)::Tuple{Int,Int})[1]

# For terminals with buffers
flush(t::TextTerminal=active_repl.t) = nothing

clear(t::TextTerminal=active_repl.t) = error("Unimplemented")
clear_line(t::TextTerminal, row) = error("Unimplemented")
clear_line(row) = clear_line(active_repl.t, row)
clear_line(t::TextTerminal=active_repl.t) = error("Unimplemented")

raw!(t::TextTerminal, raw::Bool) = error("Unimplemented")
raw!(raw::Bool) = raw!(active_repl.t, raw)

beep(t::TextTerminal=active_repl.t) = nothing
enable_bracketed_paste(t::TextTerminal=active_repl.t) = nothing
disable_bracketed_paste(t::TextTerminal=active_repl.t) = nothing

## UnixTerminal ##

abstract type UnixTerminal <: TextTerminal end

pipe_reader(t::UnixTerminal=active_repl.t) = t.in_stream::IO
pipe_writer(t::UnixTerminal=active_repl.t) = t.out_stream::IO

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
cmove_col(t::UnixTerminal, n) = (write(t.out_stream, '\r'); n > 1 && cmove_right(t, n - 1))
csave(t::UnixTerminal) = print(t.out_stream, "$(CSI)s")
crestore(t::UnixTerminal) = print(t.out_stream, "$(CSI)u")

const is_precompiling = Ref(false)
if Sys.iswindows()
    function raw!(t::TTYTerminal, raw::Bool)
        is_precompiling[] && return true
        check_open(t.in_stream)
        if Base.ispty(t.in_stream)
            run((raw ? `stty raw -echo onlcr -ocrnl opost` : `stty sane`),
                t.in_stream, t.out_stream, t.err_stream)
            true
        else
            ccall(:jl_tty_set_mode, Int32, (Ptr{Cvoid}, Int32), t.in_stream.handle::Ptr{Cvoid}, raw) != -1
        end
    end
else
    function raw!(t::TTYTerminal, raw::Bool)
        check_open(t.in_stream)
        ccall(:jl_tty_set_mode, Int32, (Ptr{Cvoid}, Int32), t.in_stream.handle::Ptr{Cvoid}, raw) != -1
    end
end

# eval some of these definitions to insert CSI as a constant string
@eval enable_bracketed_paste(t::UnixTerminal) = write(t.out_stream, $"$(CSI)?2004h")
@eval disable_bracketed_paste(t::UnixTerminal) = write(t.out_stream, $"$(CSI)?2004l")
@eval end_keypad_transmit_mode(t::UnixTerminal) = # tput rmkx
    write(t.out_stream, $"$(CSI)?1l\x1b>")

@eval clear(t::UnixTerminal) = write(t.out_stream, $"$(CSI)H$(CSI)2J")
@eval clear_line(t::UnixTerminal) = write(t.out_stream, $"\r$(CSI)0K")
beep(t::UnixTerminal) = write(t.err_stream, "\x7")

Base.displaysize(t::UnixTerminal) = displaysize(t.out_stream)

hascolor(t::TTYTerminal) = get(t.out_stream, :color, false)::Bool

# use cached value of have_color
Base.in(key_value::Pair, t::TTYTerminal) = in(key_value, pipe_writer(t))
Base.haskey(t::TTYTerminal, key) = haskey(pipe_writer(t), key)
Base.getindex(t::TTYTerminal, key) = getindex(pipe_writer(t), key)
Base.get(t::TTYTerminal, key, default) = get(pipe_writer(t), key, default)

Base.peek(t::TTYTerminal, ::Type{T}) where {T} = peek(t.in_stream, T)::T

end # module
