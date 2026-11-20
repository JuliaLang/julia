# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    mouse_reporting(on::Bool; motion = false) -> String

The escape sequences that turn mouse reports on or off, in the SGR encoding, without which
columns past 223 cannot be reported. They report presses, releases and drags; `motion` adds
the pointer moving with no button held, for a program that shows what the pointer rests on.
"""
mouse_reporting(on::Bool; motion::Bool=false) =
    on ? string("\e[?1006h", motion ? "\e[?1003h" : "\e[?1002h") :
         string(motion ? "\e[?1003l" : "\e[?1002l", "\e[?1006l")

"""
    bracketed_paste(on::Bool) -> String

The escape sequence that turns bracketed paste on or off, which makes a paste one
[`PasteEvent`](@ref) rather than keys.
"""
bracketed_paste(on::Bool) = on ? "\e[?2004h" : "\e[?2004l"

"""
    focus_reporting(on::Bool) -> String

The escape sequence that turns [`FocusEvent`](@ref)s on or off.
"""
focus_reporting(on::Bool) = on ? "\e[?1004h" : "\e[?1004l"

"""
    scheme_reports(on::Bool) -> String

The escape sequences that ask whether the terminal is dark or light, now and whenever that
changes, and for its background colour, or stop asking. The answers are
[`SchemeEvent`](@ref)s, and a terminal that does not know a question does not answer it.
"""
scheme_reports(on::Bool) = on ? "\e[?2031h\e[?996n\e]11;?\e\\" : "\e[?2031l"

"""
    ScreenModes

The modes that [`enter_screen`](@ref) put a terminal into, so that [`leave_screen`](@ref)
and [`suspend`](@ref) undo those and no others. A program that turns the mouse off for a
while sets `mouse` to match.
"""
mutable struct ScreenModes
    term::REPL.Terminals.UnixTerminal
    raw::Bool
    altscreen::Bool
    mouse::Bool
    motion::Bool
    paste::Bool
    focus::Bool
    scheme::Bool
end

"""
    enter_screen(term; altscreen = true, mouse = false, motion = false, paste = false,
                 focus = false, scheme = false) -> ScreenModes

Put `term` into raw mode with the cursor hidden, on the alternate screen unless `altscreen`
is false, with [`mouse_reporting`](@ref) (and its `motion`), [`bracketed_paste`](@ref),
[`focus_reporting`](@ref) and [`scheme_reports`](@ref) where asked for, in one write.
Undo it with [`leave_screen`](@ref) in a `finally`. A terminal that is not a tty, such as one
on an `IOBuffer`, is written to without raw mode.
"""
function enter_screen(term::REPL.Terminals.UnixTerminal; altscreen::Bool=true, mouse::Bool=false,
                      motion::Bool=false, paste::Bool=false, focus::Bool=false, scheme::Bool=false)
    raw = try
        REPL.Terminals.raw!(term, true)
    catch
        false
    end
    s = ScreenModes(term, raw, altscreen, mouse, motion, paste, focus, scheme)
    write(term.out_stream, string(altscreen ? "\e[?1049h" : "", "\e[?25l", modes_on(s)))
    return s
end

modes_on(s::ScreenModes) = string(s.mouse ? mouse_reporting(true; s.motion) : "",
                                  s.paste ? bracketed_paste(true) : "",
                                  s.focus ? focus_reporting(true) : "",
                                  s.scheme ? scheme_reports(true) : "")
modes_off(s::ScreenModes) = string(s.scheme ? scheme_reports(false) : "",
                                   s.focus ? focus_reporting(false) : "",
                                   s.paste ? bracketed_paste(false) : "",
                                   s.mouse ? mouse_reporting(false; s.motion) : "")

"""
    leave_screen(s::ScreenModes)

Undo what [`enter_screen`](@ref) did, in the reverse order. It never throws: the commonest way
here is a terminal that has gone away, and an error from the `finally` it is called in would
hide whatever brought the program there.
"""
function leave_screen(s::ScreenModes)
    out = s.term.out_stream
    try
        write(out, modes_off(s))
    catch
    end
    try
        s.raw && REPL.Terminals.raw!(s.term, false)
    catch
    end
    try
        write(out, string("\e[?25h", s.altscreen ? "\e[?1049l" : ""))
    catch
    end
    return nothing
end

"""
    suspend(f, s::ScreenModes)

Give the terminal back as it was for the duration of `f`, to run an editor or a shell in it,
then put it into the modes of `s` again. Nothing should be reading the terminal meanwhile.
The screen has to be drawn again afterwards.
"""
function suspend(f, s::ScreenModes)
    out = s.term.out_stream
    write(out, modes_off(s))
    s.raw && REPL.Terminals.raw!(s.term, false)
    write(out, string("\e[?25h", s.altscreen ? "\e[?1049l" : ""))
    try
        return f()
    finally
        write(out, string(s.altscreen ? "\e[?1049h" : "", "\e[?25l"))
        s.raw && REPL.Terminals.raw!(s.term, true)
        write(out, modes_on(s))
    end
end

"""
    write_frame(io::IO, rows, prev = nothing) -> rows

Draw `rows` as the lines of the screen from the top, in one write, writing only the rows
that differ from `prev`, the rows drawn last. A row is a string, written as it is, or an
annotated string, written in its faces where `io` has colour.

The write is held in synchronized output, for terminals that know it, so that it shows at
once. Auto-wrap is off while it is written, so a row wider than the screen is cut at its
edge rather than pushing the rest down. Each row's line is deleted and put back blank before
the row is written: xterm.js, which is VS Code's terminal, keeps a hyperlink's marker until
its line is deleted, so lines written over in place keep every link they ever showed.
"""
function write_frame(io::IO, rows::AbstractVector, prev::Union{Nothing,AbstractVector}=nothing)
    buf = IOBuffer()
    out = IOContext(buf, :color => get(io, :color, false)::Bool)
    # deleting a line outside the scroll region does nothing
    print(buf, "\e[?2026h\e[?7l\e[r")
    for (i, row) in enumerate(rows)
        prev !== nothing && i <= length(prev) && prev[i] == row && continue
        print(buf, "\e[", i, "H\e[M\e[L\e[0m")
        print(out, row)
        print(buf, "\e[0m")
    end
    print(buf, "\e[?7h\e[?2026l")
    write(io, take!(buf))
    return rows
end
