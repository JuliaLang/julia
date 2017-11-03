# This file is a part of Julia. License is MIT: https://julialang.org/license

module LineEdit

using ..Terminals

import ..Terminals: raw!, width, height, cmove, getX,
                       getY, clear_line, beep

import Base: ensureroom, peek, show, AnyDict, position

abstract type TextInterface end
abstract type ModeState end

export run_interface, Prompt, ModalInterface, transition, reset_state, edit_insert, keymap

struct ModalInterface <: TextInterface
    modes::Array{Base.LineEdit.TextInterface,1}
end

mutable struct Prompt <: TextInterface
    # A string or function to be printed as the prompt.
    prompt::Union{String,Function}
    # A string or function to be printed before the prompt. May not change the length of the prompt.
    # This may be used for changing the color, issuing other terminal escape codes, etc.
    prompt_prefix::Union{String,Function}
    # Same as prefix except after the prompt
    prompt_suffix::Union{String,Function}
    keymap_dict::Dict{Char}
    repl # ::AbstractREPL
    complete # ::REPLCompletionProvider
    on_enter::Function
    on_done::Function
    hist # ::REPLHistoryProvider
    sticky::Bool
end

show(io::IO, x::Prompt) = show(io, string("Prompt(\"", prompt_string(x.prompt), "\",...)"))

"Maximum number of entries in the kill ring queue.
Beyond this number, oldest entries are discarded first."
const KILL_RING_MAX = Ref(100)

mutable struct MIState
    interface::ModalInterface
    current_mode::TextInterface
    aborted::Bool
    mode_state::Dict
    kill_ring::Vector{String}
    kill_idx::Int
    previous_key::Vector{Char}
    key_repeats::Int
    last_action::Symbol
end
MIState(i, c, a, m) = MIState(i, c, a, m, String[], 0, Char[], 0, :begin)

function show(io::IO, s::MIState)
    print(io, "MI State (", mode(s), " active)")
end

struct InputAreaState
    num_rows::Int64
    curs_row::Int64
end

mutable struct PromptState <: ModeState
    terminal::AbstractTerminal
    p::Prompt
    input_buffer::IOBuffer
    undo_buffers::Vector{IOBuffer}
    undo_idx::Int
    ias::InputAreaState
    # indentation of lines which do not include the prompt
    # if negative, the width of the prompt is used
    indent::Int
    refresh_lock::Threads.AbstractLock
    # this would better be Threads.Atomic{Float64}, but not supported on some platforms
    beeping::Float64
end

options(s::PromptState) = isdefined(s.p, :repl) ? s.p.repl.options : Base.REPL.Options()

setmark(s) = mark(buffer(s))

# the default mark is 0
getmark(s) = max(0, buffer(s).mark)

const Region = Pair{<:Integer,<:Integer}

_region(s) = getmark(s) => position(s)
region(s) = Pair(extrema(_region(s))...)

bufend(s) = buffer(s).size

indexes(reg::Region) = first(reg)+1:last(reg)

content(s, reg::Region = 0=>bufend(s)) = String(buffer(s).data[indexes(reg)])

const REGION_ANIMATION_DURATION = Ref(0.2)

input_string(s::PromptState) = String(take!(copy(s.input_buffer)))

input_string_newlines(s::PromptState) = count(c->(c == '\n'), input_string(s))
function input_string_newlines_aftercursor(s::PromptState)
    str = input_string(s)
    isempty(str) && return 0
    rest = str[nextind(str, position(s)):end]
    return count(c->(c == '\n'), rest)
end

abstract type HistoryProvider end
abstract type CompletionProvider end

struct EmptyCompletionProvider <: CompletionProvider end
struct EmptyHistoryProvider <: HistoryProvider end

reset_state(::EmptyHistoryProvider) = nothing

complete_line(c::EmptyCompletionProvider, s) = [], true, true

terminal(s::IO) = s
terminal(s::PromptState) = s.terminal


# these may be better stored in Prompt or LineEditREPL
const BEEP_DURATION = Ref(0.2)
const BEEP_BLINK = Ref(0.2)
const BEEP_MAXDURATION = Ref(1.0)
const BEEP_COLORS = ["\e[90m"] # gray (text_colors not yet available)
const BEEP_USE_CURRENT = Ref(true)

function beep(s::PromptState, duration::Real=BEEP_DURATION[], blink::Real=BEEP_BLINK[],
              maxduration::Real=BEEP_MAXDURATION[];
              colors=BEEP_COLORS, use_current::Bool=BEEP_USE_CURRENT[])
    isinteractive() || return # some tests fail on some platforms
    s.beeping = min(s.beeping + duration, maxduration)
    @async begin
        trylock(s.refresh_lock) || return
        orig_prefix = s.p.prompt_prefix
        colors = Base.copymutable(colors)
        use_current && push!(colors, orig_prefix)
        i = 0
        while s.beeping > 0.0
            prefix = colors[mod1(i+=1, end)]
            s.p.prompt_prefix = prefix
            refresh_multi_line(s, beeping=true)
            sleep(blink)
            s.beeping -= blink
        end
        s.p.prompt_prefix = orig_prefix
        refresh_multi_line(s, beeping=true)
        s.beeping = 0.0
        unlock(s.refresh_lock)
    end
end

function cancel_beep(s::PromptState)
    # wait till beeping finishes
    while !trylock(s.refresh_lock)
        s.beeping = 0.0
        sleep(.05)
    end
    unlock(s.refresh_lock)
end

beep(::ModeState) = nothing
cancel_beep(::ModeState) = nothing

for f in [:terminal, :on_enter, :add_history, :buffer, :(Base.isempty),
          :replace_line, :refresh_multi_line, :input_string, :update_display_buffer,
          :empty_undo, :push_undo, :pop_undo, :options, :cancel_beep, :beep]
    @eval ($f)(s::MIState, args...) = $(f)(state(s), args...)
end

for f in [:edit_insert, :edit_insert_newline, :edit_backspace, :edit_move_left,
          :edit_move_right, :edit_move_word_left, :edit_move_word_right]
    @eval function ($f)(s::MIState, args...)
        $(f)(state(s), args...)
        return $(Expr(:quote, f))
    end
end


function common_prefix(completions)
    ret = ""
    c1 = completions[1]
    isempty(c1) && return ret
    i = 1
    cc, nexti = next(c1, i)
    while true
        for c in completions
            (i > endof(c) || c[i] != cc) && return ret
        end
        ret = string(ret, cc)
        i >= endof(c1) && return ret
        i = nexti
        cc, nexti = next(c1, i)
    end
end

# Show available completions
function show_completions(s::PromptState, completions)
    colmax = maximum(map(length, completions))
    num_cols = max(div(width(terminal(s)), colmax+2), 1)
    entries_per_col, r = divrem(length(completions), num_cols)
    entries_per_col += r != 0
    # skip any lines of input after the cursor
    cmove_down(terminal(s), input_string_newlines_aftercursor(s))
    println(terminal(s))
    for row = 1:entries_per_col
        for col = 0:num_cols
            idx = row + col*entries_per_col
            if idx <= length(completions)
                cmove_col(terminal(s), (colmax+2)*col)
                print(terminal(s), completions[idx])
            end
        end
        println(terminal(s))
    end
    # make space for the prompt
    for i = 1:input_string_newlines(s)
        println(terminal(s))
    end
end

# Prompt Completions
function complete_line(s::MIState)
    if complete_line(state(s), s.key_repeats)
        refresh_line(s)
        :complete_line
    else
        beep(s)
        :ignore
    end
end

function complete_line(s::PromptState, repeats)
    completions, partial, should_complete = complete_line(s.p.complete, s)
    isempty(completions) && return false
    if !should_complete
        # should_complete is false for cases where we only want to show
        # a list of possible completions but not complete, e.g. foo(\t
        show_completions(s, completions)
    elseif length(completions) == 1
        # Replace word by completion
        prev_pos = position(s)
        push_undo(s)
        edit_splice!(s, prev_pos-sizeof(partial) => prev_pos, completions[1])
    else
        p = common_prefix(completions)
        if !isempty(p) && p != partial
            # All possible completions share the same prefix, so we might as
            # well complete that
            prev_pos = position(s)
            push_undo(s)
            edit_splice!(s, prev_pos-sizeof(partial) => prev_pos, p)
        elseif repeats > 0
            show_completions(s, completions)
        end
    end
    true
end

clear_input_area(terminal, s) = (_clear_input_area(terminal, s.ias); s.ias = InputAreaState(0, 0))
clear_input_area(s) = clear_input_area(s.terminal, s)
function _clear_input_area(terminal, state::InputAreaState)
    # Go to the last line
    if state.curs_row < state.num_rows
        cmove_down(terminal, state.num_rows - state.curs_row)
    end

    # Clear lines one by one going up
    for j = 2:state.num_rows
        clear_line(terminal)
        cmove_up(terminal)
    end

    # Clear top line
    clear_line(terminal)
end

prompt_string(s::PromptState) = prompt_string(s.p)
prompt_string(p::Prompt) = prompt_string(p.prompt)
prompt_string(s::AbstractString) = s
prompt_string(f::Function) = Base.invokelatest(f)

refresh_multi_line(s::ModeState; kw...) = refresh_multi_line(terminal(s), s; kw...)
refresh_multi_line(termbuf::TerminalBuffer, s::ModeState; kw...) = refresh_multi_line(termbuf, terminal(s), s; kw...)
refresh_multi_line(termbuf::TerminalBuffer, term, s::ModeState; kw...) = (@assert term == terminal(s); refresh_multi_line(termbuf,s; kw...))
function refresh_multi_line(termbuf::TerminalBuffer, terminal::UnixTerminal, buf, state::InputAreaState, prompt = ""; indent = 0)
    _clear_input_area(termbuf, state)

    cols = width(terminal)
    curs_row = -1 # relative to prompt (1-based)
    curs_pos = -1 # 1-based column position of the cursor
    cur_row = 0   # count of the number of rows
    buf_pos = position(buf)
    line_pos = buf_pos
    # Write out the prompt string
    lindent = write_prompt(termbuf, prompt)
    # Count the '\n' at the end of the line if the terminal emulator does (specific to DOS cmd prompt)
    miscountnl = @static Sys.iswindows() ? (isa(Terminals.pipe_reader(terminal), Base.TTY) && !Base.ispty(Terminals.pipe_reader(terminal))) : false

    # Now go through the buffer line by line
    seek(buf, 0)
    moreinput = true # add a blank line if there is a trailing newline on the last line
    while moreinput
        l = readline(buf, chomp=false)
        moreinput = endswith(l, "\n")
        # We need to deal with on-screen characters, so use textwidth to compute occupied columns
        llength = textwidth(l)
        slength = sizeof(l)
        cur_row += 1
        cmove_col(termbuf, lindent + 1)
        write(termbuf, l)
        # We expect to be line after the last valid output line (due to
        # the '\n' at the end of the previous line)
        if curs_row == -1
            # in this case, we haven't yet written the cursor position
            line_pos -= slength # '\n' gets an extra pos
            if line_pos < 0 || !moreinput
                num_chars = (line_pos >= 0 ? llength : textwidth(l[1:prevind(l, line_pos + slength + 1)]))
                curs_row, curs_pos = divrem(lindent + num_chars - 1, cols)
                curs_row += cur_row
                curs_pos += 1
                # There's an issue if the cursor is after the very right end of the screen. In that case we need to
                # move the cursor to the next line, and emit a newline if needed
                if curs_pos == cols
                    # only emit the newline if the cursor is at the end of the line we're writing
                    if line_pos == 0
                        write(termbuf, "\n")
                        cur_row += 1
                    end
                    curs_row += 1
                    curs_pos = 0
                    cmove_col(termbuf, 1)
                end
            end
        end
        cur_row += div(max(lindent + llength + miscountnl - 1, 0), cols)
        lindent = indent < 0 ? lindent : indent
    end
    seek(buf, buf_pos)

    # Let's move the cursor to the right position
    # The line first
    n = cur_row - curs_row
    if n > 0
        cmove_up(termbuf, n)
    end

    #columns are 1 based
    cmove_col(termbuf, curs_pos + 1)
    # Updated cur_row,curs_row
    return InputAreaState(cur_row, curs_row)
end

function refresh_multi_line(terminal::UnixTerminal, args...; kwargs...)
    outbuf = IOBuffer()
    termbuf = TerminalBuffer(outbuf)
    ret = refresh_multi_line(termbuf, terminal, args...;kwargs...)
    # Output the entire refresh at once
    write(terminal, take!(outbuf))
    flush(terminal)
    return ret
end


# Edit functionality
is_non_word_char(c) = c in """ \t\n\"\\'`@\$><=:;|&{}()[].,+-*/?%^~"""

function reset_key_repeats(f::Function, s::MIState)
    key_repeats_sav = s.key_repeats
    try
        s.key_repeats = 0
        f()
    finally
        s.key_repeats = key_repeats_sav
    end
end

edit_exchange_point_and_mark(s::MIState) =
    edit_exchange_point_and_mark(buffer(s)) && (refresh_line(s); true)

function edit_exchange_point_and_mark(buf::IOBuffer)
    m = getmark(buf)
    m == position(buf) && return false
    mark(buf)
    seek(buf, m)
    true
end

char_move_left(s::PromptState) = char_move_left(s.input_buffer)
function char_move_left(buf::IOBuffer)
    while position(buf) > 0
        seek(buf, position(buf)-1)
        c = peek(buf)
        (((c & 0x80) == 0) || ((c & 0xc0) == 0xc0)) && break
    end
    pos = position(buf)
    c = read(buf, Char)
    seek(buf, pos)
    c
end

function edit_move_left(buf::IOBuffer)
    if position(buf) > 0
        #move to the next base UTF8 character to the left
        while true
            c = char_move_left(buf)
            if textwidth(c) != 0 || c == '\n' || position(buf) == 0
                break
            end
        end
        return true
    end
    return false
end
edit_move_left(s::PromptState) = edit_move_left(s.input_buffer) && refresh_line(s)

function edit_move_word_left(s)
    if position(s) > 0
        char_move_word_left(s.input_buffer)
        refresh_line(s)
    end
end

char_move_right(s) = char_move_right(buffer(s))
function char_move_right(buf::IOBuffer)
    !eof(buf) && read(buf, Char)
end

function char_move_word_right(buf::IOBuffer, is_delimiter=is_non_word_char)
    while !eof(buf) && is_delimiter(char_move_right(buf))
    end
    while !eof(buf)
        pos = position(buf)
        if is_delimiter(char_move_right(buf))
            seek(buf, pos)
            break
        end
    end
end

function char_move_word_left(buf::IOBuffer, is_delimiter=is_non_word_char)
    while position(buf) > 0 && is_delimiter(char_move_left(buf))
    end
    while position(buf) > 0
        pos = position(buf)
        if is_delimiter(char_move_left(buf))
            seek(buf, pos)
            break
        end
    end
end

char_move_word_right(s) = char_move_word_right(buffer(s))
char_move_word_left(s) = char_move_word_left(buffer(s))

function edit_move_right(buf::IOBuffer)
    if !eof(buf)
        # move to the next base UTF8 character to the right
        while true
            c = char_move_right(buf)
            eof(buf) && break
            pos = position(buf)
            nextc = read(buf,Char)
            seek(buf,pos)
            (textwidth(nextc) != 0 || nextc == '\n') && break
        end
        return true
    end
    return false
end
edit_move_right(s::PromptState) = edit_move_right(s.input_buffer) && refresh_line(s)

function edit_move_word_right(s)
    if !eof(s.input_buffer)
        char_move_word_right(s)
        refresh_line(s)
    end
end

## Move line up/down
# Querying the terminal is expensive, memory access is cheap
# so to find the current column, we find the offset for the start
# of the line.

function edit_move_up(buf::IOBuffer)
    npos = rsearch(buf.data, '\n', position(buf))
    npos == 0 && return false # we're in the first line
    # We're interested in character count, not byte count
    offset = length(content(buf, npos => position(buf)))
    npos2 = rsearch(buf.data, '\n', npos-1)
    seek(buf, npos2)
    for _ = 1:offset
        pos = position(buf)
        if read(buf, Char) == '\n'
            seek(buf, pos)
            break
        end
    end
    return true
end
function edit_move_up(s)
    changed = edit_move_up(buffer(s))
    changed && refresh_line(s)
    changed
end

function edit_move_down(buf::IOBuffer)
    npos = rsearch(buf.data[1:buf.size], '\n', position(buf))
    # We're interested in character count, not byte count
    offset = length(String(buf.data[(npos+1):(position(buf))]))
    npos2 = search(buf.data[1:buf.size], '\n', position(buf)+1)
    if npos2 == 0 #we're in the last line
        return false
    end
    seek(buf, npos2)
    for _ = 1:offset
        pos = position(buf)
        if eof(buf) || read(buf, Char) == '\n'
            seek(buf, pos)
            break
        end
    end
    return true
end
function edit_move_down(s)
    changed = edit_move_down(buffer(s))
    changed && refresh_line(s)
    changed
end

# splice! for IOBuffer: convert from close-open region to index, update the size,
# and keep the cursor position and mark stable with the text
# returns the removed portion as a String
function edit_splice!(s, r::Region=region(s), ins::AbstractString = "")
    A, B = first(r), last(r)
    A >= B && isempty(ins) && return String(ins)
    buf = buffer(s)
    pos = position(buf)
    if A <= pos < B
        seek(buf, A)
    elseif B <= pos
        seek(buf, pos - B + A)
    end
    if A < buf.mark  < B
        buf.mark = A
    elseif A < B <= buf.mark
        buf.mark += sizeof(ins) - B + A
    end
    ret = splice!(buf.data, A+1:B, Vector{UInt8}(ins)) # position(), etc, are 0-indexed
    buf.size = buf.size + sizeof(ins) - B + A
    seek(buf, position(buf) + sizeof(ins))
    String(ret)
end

edit_splice!(s, ins::AbstractString) = edit_splice!(s, region(s), ins)

function edit_insert(s::PromptState, c)
    push_undo(s)
    buf = s.input_buffer
    str = string(c)
    edit_insert(buf, str)
    offset = s.ias.curs_row == 1 || s.indent < 0 ?
        sizeof(prompt_string(s.p.prompt)) : s.indent
    if !('\n' in str) && eof(buf) &&
        ((position(buf) - beginofline(buf) + # size of current line
          offset + sizeof(str) - 1) < width(terminal(s)))
        # Avoid full update when appending characters to the end
        # and an update of curs_row isn't necessary (conservatively estimated)
        write(terminal(s), str)
    else
        refresh_line(s)
    end
end

function edit_insert(buf::IOBuffer, c)
    if eof(buf)
        return write(buf, c)
    else
        s = string(c)
        edit_splice!(buf, position(buf) => position(buf), s)
        return sizeof(s)
    end
end

# align: number of ' ' to insert after '\n'
# if align < 0: align like line above
function edit_insert_newline(s::PromptState, align=-1)
    push_undo(s)
    buf = buffer(s)
    if align < 0
        beg = beginofline(buf)
        align = min(findnext(_notspace, buf.data[beg+1:buf.size], 1) - 1,
                    position(buf) - beg) # indentation must not increase
        align < 0 && (align = buf.size-beg)
    end
    edit_insert(buf, '\n' * ' '^align)
    refresh_line(s)
end

# align: delete up to 4 spaces to align to a multiple of 4 chars
# adjust: also delete spaces on the right of the cursor to try to keep aligned what is
# on the right
function edit_backspace(s::PromptState, align::Bool=options(s).backspace_align,
                        adjust=options(s).backspace_adjust)
    push_undo(s)
    if edit_backspace(buffer(s), align, adjust)
        refresh_line(s)
    else
        pop_undo(s)
        beep(s)
    end
end

const _newline =  UInt8('\n')
const _space = UInt8(' ')

_notspace(c) = c != _space

beginofline(buf, pos=position(buf)) = findprev(equalto(_newline), buf.data, pos)

function endofline(buf, pos=position(buf))
    eol = findnext(equalto(_newline), buf.data[pos+1:buf.size], 1)
    eol == 0 ? buf.size : pos + eol - 1
end

function edit_backspace(buf::IOBuffer, align::Bool=false, adjust::Bool=false)
    !align && adjust &&
        throw(DomainError((align, adjust),
                          "if `adjust` is `true`, `align` must be `true`"))
    oldpos = position(buf)
    oldpos == 0 && return false
    c = char_move_left(buf)
    newpos = position(buf)
    if align && c == ' ' # maybe delete multiple spaces
        beg = beginofline(buf, newpos)
        align = textwidth(String(buf.data[1+beg:newpos])) % 4
        nonspace = findprev(_notspace, buf.data, newpos)
        if newpos - align >= nonspace
            newpos -= align
            seek(buf, newpos)
            if adjust
                spaces = findnext(_notspace, buf.data[newpos+2:buf.size], 1)
                oldpos = spaces == 0 ? buf.size :
                    buf.data[newpos+1+spaces] == _newline ? newpos+spaces :
                    newpos + min(spaces, 4)
            end
        end
    end
    edit_splice!(buf, newpos => oldpos)
    return true
end

function edit_delete(s)
    push_undo(s)
    if edit_delete(buffer(s))
        refresh_line(s)
    else
        pop_undo(s)
        beep(s)
    end
    :edit_delete
end

function edit_delete(buf::IOBuffer)
    eof(buf) && return false
    oldpos = position(buf)
    char_move_right(buf)
    edit_splice!(buf, oldpos => position(buf))
    true
end

function edit_werase(buf::IOBuffer)
    pos1 = position(buf)
    char_move_word_left(buf, isspace)
    pos0 = position(buf)
    edit_splice!(buf, pos0 => pos1)
end

function edit_werase(s::MIState)
    push_undo(s)
    if push_kill!(s, edit_werase(buffer(s)), rev=true)
        refresh_line(s)
        :edit_werase
    else
        pop_undo(s)
        :ignore
    end
end

function edit_delete_prev_word(buf::IOBuffer)
    pos1 = position(buf)
    char_move_word_left(buf)
    pos0 = position(buf)
    edit_splice!(buf, pos0 => pos1)
end

function edit_delete_prev_word(s::MIState)
    push_undo(s)
    if push_kill!(s, edit_delete_prev_word(buffer(s)), rev=true)
        refresh_line(s)
        :edit_delete_prev_word
    else
        pop_undo(s)
        :ignore
    end
end

function edit_delete_next_word(buf::IOBuffer)
    pos0 = position(buf)
    char_move_word_right(buf)
    pos1 = position(buf)
    edit_splice!(buf, pos0 => pos1)
end

function edit_delete_next_word(s)
    push_undo(s)
    if push_kill!(s, edit_delete_next_word(buffer(s)))
        refresh_line(s)
        :edit_delete_next_word
    else
        pop_undo(s)
        :ignore
    end
end

function edit_yank(s::MIState)
    if isempty(s.kill_ring)
        beep(s)
        return :ignore
    end
    setmark(s) # necessary for edit_yank_pop
    push_undo(s)
    edit_insert(buffer(s), s.kill_ring[mod1(s.kill_idx, end)])
    refresh_line(s)
    :edit_yank
end

function edit_yank_pop(s::MIState, require_previous_yank=true)
    repeat = s.last_action ∈ (:edit_yank, :edit_yank_pop)
    if require_previous_yank && !repeat || isempty(s.kill_ring)
        beep(s)
        :ignore
    else
        require_previous_yank || repeat || setmark(s)
        push_undo(s)
        edit_splice!(s, s.kill_ring[mod1(s.kill_idx-=1, end)])
        refresh_line(s)
        :edit_yank_pop
    end
end

function push_kill!(s::MIState, killed::String, concat = s.key_repeats > 0; rev=false)
    isempty(killed) && return false
    if concat && !isempty(s.kill_ring)
        s.kill_ring[end] = rev ?
            killed * s.kill_ring[end] : # keep expected order for backward deletion
            s.kill_ring[end] * killed
    else
        push!(s.kill_ring, killed)
        length(s.kill_ring) > KILL_RING_MAX[] && shift!(s.kill_ring)
    end
    s.kill_idx = endof(s.kill_ring)
    true
end

function edit_kill_line(s::MIState)
    push_undo(s)
    buf = buffer(s)
    pos = position(buf)
    endpos = endofline(buf)
    endpos == pos && buf.size > pos && (endpos += 1)
    if push_kill!(s, edit_splice!(s, pos => endpos))
        refresh_line(s)
        :edit_kill_line
    else
        pop_undo(s)
        :ignore
    end
end

function edit_copy_region(s::MIState)
    buf = buffer(s)
    push_kill!(s, content(buf, region(buf)), false) || return :ignore
    if REGION_ANIMATION_DURATION[] > 0.0
        edit_exchange_point_and_mark(s)
        sleep(REGION_ANIMATION_DURATION[])
        edit_exchange_point_and_mark(s)
    end
    :edit_copy_region
end

function edit_kill_region(s::MIState)
    push_undo(s)
    if push_kill!(s, edit_splice!(s), false)
        refresh_line(s)
        :edit_kill_region
    else
        pop_undo(s)
        :ignore
    end
end

function edit_transpose_chars(s::MIState)
    push_undo(s)
    edit_transpose_chars(buffer(s)) ? refresh_line(s) : pop_undo(s)
    :edit_transpose
end

function edit_transpose_chars(buf::IOBuffer)
    position(buf) == 0 && return false
    eof(buf) && char_move_left(buf)
    char_move_left(buf)
    pos = position(buf)
    a, b = read(buf, Char), read(buf, Char)
    seek(buf, pos)
    write(buf, b, a)
    return true
end

function edit_transpose_words(s)
    push_undo(s)
    edit_transpose_words(buffer(s)) ? refresh_line(s) : pop_undo(s)
    :edit_transpose_words
end

function edit_transpose_words(buf::IOBuffer, mode=:emacs)
    mode in [:readline, :emacs] ||
        throw(ArgumentError("`mode` must be `:readline` or `:emacs`"))
    pos = position(buf)
    if mode == :emacs
        char_move_word_left(buf)
        char_move_word_right(buf)
    end
    char_move_word_right(buf)
    e2 = position(buf)
    char_move_word_left(buf)
    b2 = position(buf)
    char_move_word_left(buf)
    b1 = position(buf)
    char_move_word_right(buf)
    e1 = position(buf)
    e1 >= b2 && (seek(buf, pos); return false)
    word2 = edit_splice!(buf, b2 => e2, content(buf, b1 => e1))
    edit_splice!(buf, b1 => e1, word2)
    seek(buf, e2)
    true
end


edit_upper_case(s) = (edit_replace_word_right(s, uppercase); :edit_upper_case)
edit_lower_case(s) = (edit_replace_word_right(s, lowercase); :edit_lower_case)
edit_title_case(s) = (edit_replace_word_right(s, ucfirst);   :edit_title_case)

function edit_replace_word_right(s, replace::Function)
    push_undo(s)
    edit_replace_word_right(buffer(s), replace) ? refresh_line(s) : pop_undo(s)
end

function edit_replace_word_right(buf::IOBuffer, replace::Function)
    # put the cursor at the beginning of the next word
    skipchars(buf, is_non_word_char)
    b = position(buf)
    char_move_word_right(buf)
    e = position(buf)
    e == b && return false
    edit_splice!(buf, b => e, replace(content(buf, b => e)))
    true
end

edit_clear(buf::IOBuffer) = truncate(buf, 0)

function edit_clear(s::MIState)
    push_undo(s)
    if push_kill!(s, edit_splice!(s, 0 => bufend(s)), false)
        refresh_line(s)
        :edit_clear
    else
        pop_undo(s)
        :ignore
    end
end

function replace_line(s::PromptState, l::IOBuffer)
    empty_undo(s)
    s.input_buffer = copy(l)
end

function replace_line(s::PromptState, l, keep_undo=false)
    keep_undo || empty_undo(s)
    s.input_buffer.ptr = 1
    s.input_buffer.size = 0
    write(s.input_buffer, l)
end

history_prev(::EmptyHistoryProvider) = ("", false)
history_next(::EmptyHistoryProvider) = ("", false)
history_first(::EmptyHistoryProvider) = ("", false)
history_last(::EmptyHistoryProvider) = ("", false)
history_search(::EmptyHistoryProvider, args...) = false
add_history(::EmptyHistoryProvider, s) = nothing
add_history(s::PromptState) = add_history(mode(s).hist, s)
history_next_prefix(s, hist, prefix) = false
history_prev_prefix(s, hist, prefix) = false

function history_prev(s, hist)
    l, ok = history_prev(mode(s).hist)
    if ok
        replace_line(s, l)
        move_input_start(s)
        refresh_line(s)
    else
        beep(s)
    end
end
function history_next(s, hist)
    l, ok = history_next(mode(s).hist)
    if ok
        replace_line(s, l)
        move_input_end(s)
        refresh_line(s)
    else
        beep(s)
    end
end

refresh_line(s) = refresh_multi_line(s)
refresh_line(s, termbuf) = refresh_multi_line(termbuf, s)

default_completion_cb(::IOBuffer) = []
default_enter_cb(_) = true

write_prompt(terminal, s::PromptState) = write_prompt(terminal, s.p)

function write_prompt(terminal, p::Prompt)
    prefix = prompt_string(p.prompt_prefix)
    suffix = prompt_string(p.prompt_suffix)
    write(terminal, prefix)
    write(terminal, Base.text_colors[:bold])
    width = write_prompt(terminal, p.prompt)
    write(terminal, Base.text_colors[:normal])
    write(terminal, suffix)
    width
end

# returns the width of the written prompt
function write_prompt(terminal, s::Union{AbstractString,Function})
    promptstr = prompt_string(s)
    write(terminal, promptstr)
    textwidth(promptstr)
end

### Keymap Support

const wildcard = Char(0x0010f7ff) # "Private Use" Char

normalize_key(key::Char) = string(key)
normalize_key(key::Integer) = normalize_key(Char(key))
function normalize_key(key::AbstractString)
    wildcard in key && error("Matching Char(0x0010f7ff) not supported.")
    buf = IOBuffer()
    i = start(key)
    while !done(key, i)
        c, i = next(key, i)
        if c == '*'
            write(buf, wildcard)
        elseif c == '^'
            c, i = next(key, i)
            write(buf, uppercase(c)-64)
        elseif c == '\\'
            c, i = next(key, i)
            if c == 'C'
                c, i = next(key, i)
                @assert c == '-'
                c, i = next(key, i)
                write(buf, uppercase(c)-64)
            elseif c == 'M'
                c, i = next(key, i)
                @assert c == '-'
                c, i = next(key, i)
                write(buf, '\e')
                write(buf, c)
            end
        else
            write(buf, c)
        end
    end
    return String(take!(buf))
end

function normalize_keys(keymap::Dict)
    ret = Dict{Any,Any}()
    for (k,v) in keymap
        normalized = normalize_key(k)
        if haskey(ret,normalized)
            error("""Multiple spellings of a key in a single keymap
                     (\"$k\" conflicts with existing mapping)""")
        end
        ret[normalized] = v
    end
    return ret
end

function add_nested_key!(keymap::Dict, key, value; override = false)
    i = start(key)
    while !done(key, i)
        c, i = next(key, i)
        if !override && c in keys(keymap) && (done(key, i) || !isa(keymap[c], Dict))
            error("Conflicting definitions for keyseq " * escape_string(key) *
                  " within one keymap")
        end
        if done(key, i)
            keymap[c] = value
            break
        elseif !(c in keys(keymap) && isa(keymap[c], Dict))
            keymap[c] = Dict{Char,Any}()
        end
        keymap = keymap[c]
    end
end

# Redirect a key as if `seq` had been the keysequence instead in a lazy fashion.
# This is different from the default eager redirect, which only looks at the current and lower
# layers of the stack.
struct KeyAlias
    seq::String
    KeyAlias(seq) = new(normalize_key(seq))
end

function match_input(k::Function, s, term, cs, keymap)
    update_key_repeats(s, cs)
    return keymap_fcn(k, String(cs))
end

match_input(k::Void, s, term, cs, keymap) = (s,p) -> return :ok
match_input(k::KeyAlias, s, term, cs, keymap) =
    match_input(keymap, s, IOBuffer(k.seq), Char[], keymap)

function match_input(k::Dict, s, term=terminal(s), cs=Char[], keymap = k)
    # if we run out of characters to match before resolving an action,
    # return an empty keymap function
    eof(term) && return keymap_fcn(nothing, "")
    c = read(term, Char)
    # Ignore any `wildcard` as this is used as a
    # placeholder for the wildcard (see normalize_key("*"))
    c == wildcard && return keymap_fcn(nothing, "")
    push!(cs, c)
    key = haskey(k, c) ? c : wildcard
    # if we don't match on the key, look for a default action then fallback on 'nothing' to ignore
    return match_input(get(k, key, nothing), s, term, cs, keymap)
end

keymap_fcn(f::Void, c) = (s, p) -> return :ok
function keymap_fcn(f::Function, c)
    return function (s, p)
        r = eval(Expr(:call,f,s, p, c))
        if isa(r, Symbol)
            return r
        else
            return :ok
        end
    end
end

update_key_repeats(s, keystroke) = nothing
function update_key_repeats(s::MIState, keystroke)
    s.key_repeats  = s.previous_key == keystroke ? s.key_repeats + 1 : 0
    s.previous_key = keystroke
    return
end


## Conflict fixing
# Consider a keymap of the form
#
# {
#   "**" => f
#   "ab" => g
# }
#
# Naively this is transformed into a tree as
#
# {
#   '*' => {
#       '*' => f
#   }
#   'a' => {
#       'b' => g
#   }
# }
#
# However, that's not what we want, because now "ac" is
# is not defined. We need to fix this up and turn it into
#
# {
#   '*' => {
#       '*' => f
#   }
#   'a' => {
#       '*' => f
#       'b' => g
#   }
# }
#
# i.e. copy over the appropraite default subdict
#

# deep merge where target has higher precedence
function keymap_merge!(target::Dict, source::Dict)
    for k in keys(source)
        if !haskey(target, k)
            target[k] = source[k]
        elseif isa(target[k], Dict)
            keymap_merge!(target[k], source[k])
        else
            # Ignore, target has higher precedence
        end
    end
end

fixup_keymaps!(d, l, s, sk) = nothing
function fixup_keymaps!(dict::Dict, level, s, subkeymap)
    if level > 0
        for d in values(dict)
            fixup_keymaps!(d, level-1, s, subkeymap)
        end
    else
        if haskey(dict, s)
            if isa(dict[s], Dict) && isa(subkeymap, Dict)
                keymap_merge!(dict[s], subkeymap)
            end
        else
            dict[s] = deepcopy(subkeymap)
        end
    end
end

function add_specialisations(dict, subdict, level)
    default_branch = subdict[wildcard]
    if isa(default_branch, Dict)
        # Go through all the keymaps in the default branch
        # and copy them over to dict
        for s in keys(default_branch)
            s == wildcard && add_specialisations(dict, default_branch, level+1)
            fixup_keymaps!(dict, level, s, default_branch[s])
        end
    end
end

postprocess!(others) = nothing
function postprocess!(dict::Dict)
    # needs to be done first for every branch
    if haskey(dict, wildcard)
        add_specialisations(dict, dict, 1)
    end
    for (k,v) in dict
        k == wildcard && continue
        postprocess!(v)
    end
end

function getEntry(keymap,key)
    v = keymap
    for c in key
        if !haskey(v,c)
            return nothing
        end
        v = v[c]
    end
    return v
end

# `target` is the total keymap being built up, already being a nested tree of Dicts.
# source is the keymap specified by the user (with normalized keys)
function keymap_merge(target,source)
    ret = copy(target)
    direct_keys = filter(p -> isa(p.second, Union{Function, KeyAlias, Void}), source)
    # first direct entries
    for key in keys(direct_keys)
        add_nested_key!(ret, key, source[key]; override = true)
    end
    # then redirected entries
    for key in setdiff(keys(source), keys(direct_keys))
        # We first resolve redirects in the source
        value = source[key]
        visited = Vector{Any}(0)
        while isa(value, Union{Char,AbstractString})
            value = normalize_key(value)
            if value in visited
                error("Eager redirection cycle detected for key " * escape_string(key))
            end
            push!(visited,value)
            if !haskey(source,value)
                break
            end
            value = source[value]
        end

        if isa(value, Union{Char,AbstractString})
            value = getEntry(ret, value)
            if value === nothing
                error("Could not find redirected value " * escape_string(source[key]))
            end
        end
        add_nested_key!(ret, key, value; override = true)
    end
    ret
end

function keymap_unify(keymaps)
    ret = Dict{Char,Any}()
    for keymap in keymaps
        ret = keymap_merge(ret, keymap)
    end
    postprocess!(ret)
    return ret
end

function validate_keymap(keymap)
    for key in keys(keymap)
        visited_keys = Any[key]
        v = getEntry(keymap,key)
        while isa(v,KeyAlias)
            if v.seq in visited_keys
                error("Alias cycle detected in keymap")
            end
            push!(visited_keys,v.seq)
            v = getEntry(keymap,v.seq)
        end
    end
end

function keymap(keymaps::Array{<:Dict})
    # keymaps is a vector of prioritized keymaps, with highest priority first
    ret = keymap_unify(map(normalize_keys, reverse(keymaps)))
    validate_keymap(ret)
    ret
end

const escape_defaults = merge!(
    AnyDict(Char(i) => nothing for i=vcat(0:26, 28:31)), # Ignore control characters by default
    AnyDict( # And ignore other escape sequences by default
        "\e*" => nothing,
        "\e[*" => nothing,
        "\eO*" => nothing,
        # Also ignore extended escape sequences
        # TODO: Support ranges of characters
        "\e[1**" => nothing,
        "\e[2**" => nothing,
        "\e[3**" => nothing,
        "\e[4**" => nothing,
        "\e[5**" => nothing,
        "\e[6**" => nothing,
        # less commonly used VT220 editing keys
        "\e[2~" => nothing, # insert
        "\e[3~" => nothing, # delete
        "\e[5~" => nothing, # page up
        "\e[6~" => nothing, # page down
        # These are different spellings of arrow keys, home keys, etc.
        # and should always do the same as the canonical key sequence
        "\e[1~" => KeyAlias("\e[H"), # home
        "\e[4~" => KeyAlias("\e[F"), # end
        "\e[7~" => KeyAlias("\e[H"), # home
        "\e[8~" => KeyAlias("\e[F"), # end
        "\eOA"  => KeyAlias("\e[A"),
        "\eOB"  => KeyAlias("\e[B"),
        "\eOC"  => KeyAlias("\e[C"),
        "\eOD"  => KeyAlias("\e[D"),
        "\eOH"  => KeyAlias("\e[H"),
        "\eOF"  => KeyAlias("\e[F"),
    ),
    # set mode commands
    AnyDict("\e[$(c)h" => nothing for c in 1:20),
    # reset mode commands
    AnyDict("\e[$(c)l" => nothing for c in 1:20)
    )

function write_response_buffer(s::PromptState, data)
    offset = s.input_buffer.ptr
    ptr = data.response_buffer.ptr
    seek(data.response_buffer, 0)
    write(s.input_buffer, read(data.response_buffer, String))
    s.input_buffer.ptr = offset + ptr - 2
    data.response_buffer.ptr = ptr
    refresh_line(s)
end

mutable struct SearchState <: ModeState
    terminal::AbstractTerminal
    histprompt # ::HistoryPrompt
    #rsearch (true) or ssearch (false)
    backward::Bool
    query_buffer::IOBuffer
    response_buffer::IOBuffer
    ias::InputAreaState
    #The prompt whose input will be replaced by the matched history
    parent::Prompt
    SearchState(terminal, histprompt, backward, query_buffer, response_buffer) =
        new(terminal, histprompt, backward, query_buffer, response_buffer, InputAreaState(0,0))
end

terminal(s::SearchState) = s.terminal

function update_display_buffer(s::SearchState, data)
    history_search(data.histprompt.hp, data.query_buffer, data.response_buffer, data.backward, false) || beep(s)
    refresh_line(s)
end

function history_next_result(s::MIState, data::SearchState)
    history_search(data.histprompt.hp, data.query_buffer, data.response_buffer, data.backward, true) || beep(s)
    refresh_line(data)
end

function history_set_backward(s::SearchState, backward)
    s.backward = backward
end

input_string(s::SearchState) = String(take!(copy(s.query_buffer)))

function reset_state(s::SearchState)
    if s.query_buffer.size != 0
        s.query_buffer.size = 0
        s.query_buffer.ptr = 1
    end
    if s.response_buffer.size != 0
        s.response_buffer.size = 0
        s.response_buffer.ptr = 1
    end
    reset_state(s.histprompt.hp)
end

mutable struct HistoryPrompt{T<:HistoryProvider} <: TextInterface
    hp::T
    complete # ::CompletionProvider
    keymap_dict::Dict{Char,Any}
    HistoryPrompt{T}(hp) where T<:HistoryProvider = new(hp, EmptyCompletionProvider())
end

HistoryPrompt(hp::T) where T<:HistoryProvider = HistoryPrompt{T}(hp)
init_state(terminal, p::HistoryPrompt) = SearchState(terminal, p, true, IOBuffer(), IOBuffer())

mutable struct PrefixSearchState <: ModeState
    terminal::AbstractTerminal
    histprompt # ::HistoryPrompt
    prefix::String
    response_buffer::IOBuffer
    ias::InputAreaState
    indent::Int
    # The modal interface state, if present
    mi::MIState
    #The prompt whose input will be replaced by the matched history
    parent::Prompt
    PrefixSearchState(terminal, histprompt, prefix, response_buffer) =
        new(terminal, histprompt, prefix, response_buffer, InputAreaState(0,0), 0)
end

function show(io::IO, s::PrefixSearchState)
    print(io, "PrefixSearchState ", isdefined(s,:parent) ?
     string("(", s.parent, " active)") : "(no parent)", " for ",
     isdefined(s,:mi) ? s.mi : "no MI")
end

function refresh_multi_line(termbuf::TerminalBuffer, terminal::UnixTerminal,
                            s::Union{PromptState,PrefixSearchState}; beeping=false)
    beeping || cancel_beep(s)
    s.ias = refresh_multi_line(termbuf, terminal, buffer(s), s.ias, s, indent = s.indent)
end

input_string(s::PrefixSearchState) = String(take!(copy(s.response_buffer)))

# a meta-prompt that presents itself as parent_prompt, but which has an independent keymap
# for prefix searching
mutable struct PrefixHistoryPrompt{T<:HistoryProvider} <: TextInterface
    hp::T
    parent_prompt::Prompt
    complete # ::CompletionProvider
    keymap_dict::Dict{Char,Any}
    PrefixHistoryPrompt{T}(hp, parent_prompt) where T<:HistoryProvider =
        new(hp, parent_prompt, EmptyCompletionProvider())
end

PrefixHistoryPrompt(hp::T, parent_prompt) where T<:HistoryProvider = PrefixHistoryPrompt{T}(hp, parent_prompt)
init_state(terminal, p::PrefixHistoryPrompt) = PrefixSearchState(terminal, p, "", IOBuffer())

write_prompt(terminal, s::PrefixSearchState) = write_prompt(terminal, s.histprompt.parent_prompt)
prompt_string(s::PrefixSearchState) = prompt_string(s.histprompt.parent_prompt.prompt)

terminal(s::PrefixSearchState) = s.terminal

function reset_state(s::PrefixSearchState)
    if s.response_buffer.size != 0
        s.response_buffer.size = 0
        s.response_buffer.ptr = 1
    end
    reset_state(s.histprompt.hp)
end

function transition(f::Function, s::PrefixSearchState, mode)
    if isdefined(s, :mi)
        transition(s.mi, mode)
    end
    s.parent = mode
    s.histprompt.parent_prompt = mode
    if isdefined(s, :mi)
        transition(f, s.mi, s.histprompt)
    else
        f()
    end
end

replace_line(s::PrefixSearchState, l::IOBuffer) = s.response_buffer = l
function replace_line(s::PrefixSearchState, l)
    s.response_buffer.ptr = 1
    s.response_buffer.size = 0
    write(s.response_buffer, l)
end

function refresh_multi_line(termbuf::TerminalBuffer, s::SearchState)
    buf = IOBuffer()
    unsafe_write(buf, pointer(s.query_buffer.data), s.query_buffer.ptr-1)
    write(buf, "': ")
    offset = buf.ptr
    ptr = s.response_buffer.ptr
    seek(s.response_buffer, 0)
    write(buf, read(s.response_buffer, String))
    buf.ptr = offset + ptr - 1
    s.response_buffer.ptr = ptr
    s.ias = refresh_multi_line(termbuf, s.terminal, buf, s.ias, s.backward ? "(reverse-i-search)`" : "(forward-i-search)`")
end

state(s::MIState, p=mode(s)) = s.mode_state[p]
state(s::PromptState, p=mode(s)) = (@assert s.p == p; s)
mode(s::MIState) = s.current_mode
mode(s::PromptState) = s.p
mode(s::SearchState) = @assert false
mode(s::PrefixSearchState) = s.histprompt.parent_prompt

# Search Mode completions
function complete_line(s::SearchState, repeats)
    completions, partial, should_complete = complete_line(s.histprompt.complete, s)
    # For now only allow exact completions in search mode
    if length(completions) == 1
        prev_pos = position(s)
        push_undo(s)
        edit_splice!(s, prev_pos-sizeof(partial) => prev_pos, completions[1])
    end
end

function accept_result(s, p)
    parent = state(s, p).parent
    transition(s, parent) do
        replace_line(state(s, parent), state(s, p).response_buffer)
    end
end

function copybuf!(dst::IOBuffer, src::IOBuffer)
    n = src.size
    ensureroom(dst, n)
    copy!(dst.data, 1, src.data, 1, n)
    dst.size = src.size
    dst.ptr = src.ptr
end

function enter_search(s::MIState, p::HistoryPrompt, backward::Bool)
    # a bit of hack to help fix #6325
    buf = copy(buffer(s))
    parent = mode(s)
    p.hp.last_mode = mode(s)
    p.hp.last_buffer = buf

    transition(s, p) do
        ss = state(s, p)
        ss.parent = parent
        ss.backward = backward
        truncate(ss.query_buffer, 0)
        copybuf!(ss.response_buffer, buf)
    end
end

function enter_prefix_search(s::MIState, p::PrefixHistoryPrompt, backward::Bool)
    buf = copy(buffer(s))
    parent = mode(s)

    transition(s, p) do
        pss = state(s, p)
        pss.parent = parent
        pss.histprompt.parent_prompt = parent
        pss.prefix = String(buf.data[1:position(buf)])
        copybuf!(pss.response_buffer, buf)
        pss.indent = state(s, parent).indent
        pss.mi = s
    end
    pss = state(s, p)
    if backward
        history_prev_prefix(pss, pss.histprompt.hp, pss.prefix)
    else
        history_next_prefix(pss, pss.histprompt.hp, pss.prefix)
    end
end

function setup_search_keymap(hp)
    p = HistoryPrompt(hp)
    pkeymap = AnyDict(
        "^R"      => (s,data,c)->(history_set_backward(data, true); history_next_result(s, data)),
        "^S"      => (s,data,c)->(history_set_backward(data, false); history_next_result(s, data)),
        '\r'      => (s,o...)->accept_result(s, p),
        '\n'      => '\r',
        # Limited form of tab completions
        '\t'      => (s,data,c)->(complete_line(s); update_display_buffer(s, data)),
        "^L"      => (s,data,c)->(Terminals.clear(terminal(s)); update_display_buffer(s, data)),

        # Backspace/^H
        '\b'      => (s,data,c)->(edit_backspace(data.query_buffer) ?
                        update_display_buffer(s, data) : beep(s)),
        127       => KeyAlias('\b'),
        # Meta Backspace
        "\e\b"    => (s,data,c)->(isempty(edit_delete_prev_word(data.query_buffer)) ?
                                  beep(s) : update_display_buffer(s, data)),
        "\e\x7f"  => "\e\b",
        # Word erase to whitespace
        "^W"      => (s,data,c)->(isempty(edit_werase(data.query_buffer)) ?
                                  beep(s) : update_display_buffer(s, data)),
        # ^C and ^D
        "^C"      => (s,data,c)->(edit_clear(data.query_buffer);
                       edit_clear(data.response_buffer);
                       update_display_buffer(s, data);
                       reset_state(data.histprompt.hp);
                       transition(s, data.parent)),
        "^D"      => "^C",
        # Other ways to cancel search mode (it's difficult to bind \e itself)
        "^G"      => "^C",
        "\e\e"    => "^C",
        "^K"      => (s,o...)->transition(s, state(s, p).parent),
        "^Y"      => (s,data,c)->(edit_yank(s); update_display_buffer(s, data)),
        "^U"      => (s,data,c)->(edit_clear(data.query_buffer);
                     edit_clear(data.response_buffer);
                     update_display_buffer(s, data)),
        # Right Arrow
        "\e[C"    => (s,o...)->(accept_result(s, p); edit_move_right(s)),
        # Left Arrow
        "\e[D"    => (s,o...)->(accept_result(s, p); edit_move_left(s)),
        # Up Arrow
        "\e[A"    => (s,o...)->(accept_result(s, p); edit_move_up(s)),
        # Down Arrow
        "\e[B"    => (s,o...)->(accept_result(s, p); edit_move_down(s)),
        "^B"      => (s,o...)->(accept_result(s, p); edit_move_left(s)),
        "^F"      => (s,o...)->(accept_result(s, p); edit_move_right(s)),
        # Meta B
        "\eb"     => (s,o...)->(accept_result(s, p); edit_move_word_left(s)),
        # Meta F
        "\ef"     => (s,o...)->(accept_result(s, p); edit_move_word_right(s)),
        # Ctrl-Left Arrow
        "\e[1;5D" => "\eb",
        # Ctrl-Left Arrow on rxvt
        "\eOd" => "\eb",
        # Ctrl-Right Arrow
        "\e[1;5C" => "\ef",
        # Ctrl-Right Arrow on rxvt
        "\eOc" => "\ef",
        "^A"         => (s,o...)->(accept_result(s, p); move_line_start(s); refresh_line(s)),
        "^E"         => (s,o...)->(accept_result(s, p); move_line_end(s); refresh_line(s)),
        "^Z"      => (s,o...)->(return :suspend),
        # Try to catch all Home/End keys
        "\e[H"    => (s,o...)->(accept_result(s, p); move_input_start(s); refresh_line(s)),
        "\e[F"    => (s,o...)->(accept_result(s, p); move_input_end(s); refresh_line(s)),
        # Use ^N and ^P to change search directions and iterate through results
        "^N"      => (s,data,c)->(history_set_backward(data, false); history_next_result(s, data)),
        "^P"      => (s,data,c)->(history_set_backward(data, true); history_next_result(s, data)),
        # Bracketed paste mode
        "\e[200~" => (s,data,c)-> begin
            ps = state(s, mode(s))
            str = readuntil(ps.terminal, "\e[201~")
            input = str[1:prevind(str, end-5)]
            edit_insert(data.query_buffer, input); update_display_buffer(s, data)
        end,
        "*"       => (s,data,c)->(edit_insert(data.query_buffer, c); update_display_buffer(s, data))
    )
    p.keymap_dict = keymap([pkeymap, escape_defaults])
    skeymap = AnyDict(
        "^R"    => (s,o...)->(enter_search(s, p, true)),
        "^S"    => (s,o...)->(enter_search(s, p, false)),
    )
    (p, skeymap)
end

keymap(state, p::Union{HistoryPrompt,PrefixHistoryPrompt}) = p.keymap_dict
keymap_data(state, ::Union{HistoryPrompt, PrefixHistoryPrompt}) = state

Base.isempty(s::PromptState) = s.input_buffer.size == 0

on_enter(s::PromptState) = s.p.on_enter(s)

move_input_start(s) = (seek(buffer(s), 0))
move_input_end(buf::IOBuffer) = seekend(buf)
move_input_end(s) = move_input_end(buffer(s))
function move_line_start(s::MIState)
    buf = buffer(s)
    curpos = position(buf)
    curpos == 0 && return
    if s.key_repeats > 0
        move_input_start(s)
    else
        seek(buf, rsearch(buf.data, '\n', curpos))
    end
    :move_line_start
end
function move_line_end(s::MIState)
    s.key_repeats > 0 ?
        move_input_end(s) :
        move_line_end(buffer(s))
    :move_line_end
end
function move_line_end(buf::IOBuffer)
    eof(buf) && return
    pos = search(buf.data, '\n', position(buf)+1)
    if pos == 0
        move_input_end(buf)
        return
    end
    seek(buf, pos-1)
end

function commit_line(s)
    cancel_beep(s)
    move_input_end(s)
    refresh_line(s)
    println(terminal(s))
    add_history(s)
    state(s, mode(s)).ias = InputAreaState(0, 0)
end

"""
`Base.LineEdit.tabwidth` controls the presumed tab width of code pasted into the REPL.

You can modify it by doing `@eval Base.LineEdit tabwidth = 4`, for example.

Must satisfy `0 < tabwidth <= 16`.
"""
global tabwidth = 8

function bracketed_paste(s)
    ps = state(s, mode(s))
    str = readuntil(ps.terminal, "\e[201~")
    input = str[1:prevind(str, end-5)]
    input = replace(input, '\r', '\n')
    if position(buffer(s)) == 0
        indent = Base.indentation(input; tabwidth=tabwidth)[1]
        input = Base.unindent(input, indent; tabwidth=tabwidth)
    end
    return replace(input, '\t', " "^tabwidth)
end

function tab_should_complete(s)
    # Yes, we are ignoring the possiblity
    # the we could be in the middle of a multi-byte
    # sequence, here but that's ok, since any
    # whitespace we're interested in is only one byte
    buf = buffer(s)
    pos = position(buf)
    pos == 0 && return true
    c = buf.data[pos]
    c != _newline && c != UInt8('\t') &&
        # hack to allow path completion in cmds
        # after a space, e.g., `cd <tab>`, while still
        # allowing multiple indent levels
        (c != _space || pos <= 3 || buf.data[pos-1] != _space)
end

# jump_spaces: if cursor is on a ' ', move it to the first non-' ' char on the right
# if `delete_trailing`, ignore trailing ' ' by deleting them
function edit_tab(s::MIState, jump_spaces=false, delete_trailing=jump_spaces)
    if tab_should_complete(s)
        complete_line(s)
    else
        push_undo(s)
        edit_insert_tab(buffer(s), jump_spaces, delete_trailing) || pop_undo(s)
        refresh_line(s)
        :edit_insert_tab
    end
end

# return true iff the content of the buffer is modified
# return false when only the position changed
function edit_insert_tab(buf::IOBuffer, jump_spaces=false, delete_trailing=jump_spaces)
    i = position(buf)
    if jump_spaces && i < buf.size && buf.data[i+1] == _space
        spaces = findnext(_notspace, buf.data[i+1:buf.size], 1)
        if delete_trailing && (spaces == 0 || buf.data[i+spaces] == _newline)
            edit_splice!(buf, i => (spaces == 0 ? buf.size : i+spaces-1))
        else
            jump = spaces == 0 ? buf.size : i+spaces-1
            seek(buf, jump)
            return false
        end
    end
    # align to multiples of 4:
    align = 4 - textwidth(String(buf.data[1+beginofline(buf, i):i])) % 4
    edit_insert(buf, ' '^align)
    return true
end


const default_keymap =
AnyDict(
    # Tab
    '\t' => (s,o...)->edit_tab(s, true),
    # Enter
    '\r' => (s,o...)->begin
        if on_enter(s) || (eof(buffer(s)) && s.key_repeats > 1)
            commit_line(s)
            return :done
        else
            edit_insert_newline(s)
        end
    end,
    '\n' => KeyAlias('\r'),
    # Backspace/^H
    '\b' => (s,o...)->edit_backspace(s),
    127 => KeyAlias('\b'),
    # Meta Backspace
    "\e\b" => (s,o...)->edit_delete_prev_word(s),
    "\e\x7f" => "\e\b",
    # ^D
    "^D" => (s,o...)->begin
        if buffer(s).size > 0
            edit_delete(s)
        else
            println(terminal(s))
            return :abort
        end
    end,
    # Ctrl-Space
    "\0" => (s,o...)->setmark(s),
    "^X^X" => (s,o...)->edit_exchange_point_and_mark(s),
    "^B" => (s,o...)->edit_move_left(s),
    "^F" => (s,o...)->edit_move_right(s),
    "^P" => (s,o...)->edit_move_up(s),
    "^N" => (s,o...)->edit_move_down(s),
    # Meta B
    "\eb" => (s,o...)->edit_move_word_left(s),
    # Meta F
    "\ef" => (s,o...)->edit_move_word_right(s),
    # Ctrl-Left Arrow
    "\e[1;5D" => "\eb",
    # Ctrl-Left Arrow on rxvt
    "\eOd" => "\eb",
    # Ctrl-Right Arrow
    "\e[1;5C" => "\ef",
    # Ctrl-Right Arrow on rxvt
    "\eOc" => "\ef",
    # Meta Enter
    "\e\r" => (s,o...)->edit_insert_newline(s),
    "\e\n" => "\e\r",
    "^_" => (s,o...)->edit_undo!(s),
    "\e_" => (s,o...)->edit_redo!(s),
    # Simply insert it into the buffer by default
    "*" => (s,data,c)->(edit_insert(s, c)),
    "^U" => (s,o...)->edit_clear(s),
    "^K" => (s,o...)->edit_kill_line(s),
    "^Y" => (s,o...)->edit_yank(s),
    "\ey" => (s,o...)->edit_yank_pop(s),
    "\ew" => (s,o...)->edit_copy_region(s),
    "\eW" => (s,o...)->edit_kill_region(s),
    "^A" => (s,o...)->(move_line_start(s); refresh_line(s)),
    "^E" => (s,o...)->(move_line_end(s); refresh_line(s)),
    # Try to catch all Home/End keys
    "\e[H"  => (s,o...)->(move_input_start(s); refresh_line(s)),
    "\e[F"  => (s,o...)->(move_input_end(s); refresh_line(s)),
    "^L" => (s,o...)->(Terminals.clear(terminal(s)); refresh_line(s)),
    "^W" => (s,o...)->edit_werase(s),
    # Meta D
    "\ed" => (s,o...)->edit_delete_next_word(s),
    "^C" => (s,o...)->begin
        try # raise the debugger if present
            ccall(:jl_raise_debugger, Int, ())
        end
        cancel_beep(s)
        move_input_end(s)
        refresh_line(s)
        print(terminal(s), "^C\n\n")
        transition(s, :reset)
        refresh_line(s)
    end,
    "^Z" => (s,o...)->(return :suspend),
    # Right Arrow
    "\e[C" => (s,o...)->edit_move_right(s),
    # Left Arrow
    "\e[D" => (s,o...)->edit_move_left(s),
    # Up Arrow
    "\e[A" => (s,o...)->edit_move_up(s),
    # Down Arrow
    "\e[B" => (s,o...)->edit_move_down(s),
    # Delete
    "\e[3~" => (s,o...)->edit_delete(s),
    # Bracketed Paste Mode
    "\e[200~" => (s,o...)->begin
        input = bracketed_paste(s)
        edit_insert(s, input)
    end,
    "^T" => (s,o...)->edit_transpose_chars(s),
    "\et" => (s,o...)->edit_transpose_words(s),
    "\eu" => (s,o...)->edit_upper_case(s),
    "\el" => (s,o...)->edit_lower_case(s),
    "\ec" => (s,o...)->edit_title_case(s),
)

const history_keymap = AnyDict(
    "^P" => (s,o...)->(edit_move_up(s) || history_prev(s, mode(s).hist)),
    "^N" => (s,o...)->(edit_move_down(s) || history_next(s, mode(s).hist)),
    "\ep" => (s,o...)->(history_prev(s, mode(s).hist)),
    "\en" => (s,o...)->(history_next(s, mode(s).hist)),
    # Up Arrow
    "\e[A" => (s,o...)->(edit_move_up(s) || history_prev(s, mode(s).hist)),
    # Down Arrow
    "\e[B" => (s,o...)->(edit_move_down(s) || history_next(s, mode(s).hist)),
    # Page Up
    "\e[5~" => (s,o...)->(history_prev(s, mode(s).hist)),
    # Page Down
    "\e[6~" => (s,o...)->(history_next(s, mode(s).hist)),
    "\e<" => (s,o...)->(history_first(s, mode(s).hist)),
    "\e>" => (s,o...)->(history_last(s, mode(s).hist)),
)

const prefix_history_keymap = merge!(
    AnyDict(
        "^P" => (s,data,c)->history_prev_prefix(data, data.histprompt.hp, data.prefix),
        "^N" => (s,data,c)->history_next_prefix(data, data.histprompt.hp, data.prefix),
        # Up Arrow
        "\e[A" => (s,data,c)->history_prev_prefix(data, data.histprompt.hp, data.prefix),
        # Down Arrow
        "\e[B" => (s,data,c)->history_next_prefix(data, data.histprompt.hp, data.prefix),
        # by default, pass thru to the parent mode
        "*"    => (s,data,c)->begin
            accept_result(s, data.histprompt);
            ps = state(s, mode(s))
            map = keymap(ps, mode(s))
            match_input(map, s, IOBuffer(c))(s, keymap_data(ps, mode(s)))
        end,
        # match escape sequences for pass thru
        "\e*" => "*",
        "\e[*" => "*",
        "\eO*"  => "*",
        "\e[1;5*" => "*", # Ctrl-Arrow
        "\e[200~" => "*"
    ),
    # VT220 editing commands
    AnyDict("\e[$(n)~" => "*" for n in 1:8),
    # set mode commands
    AnyDict("\e[$(c)h" => "*" for c in 1:20),
    # reset mode commands
    AnyDict("\e[$(c)l" => "*" for c in 1:20)
)

function setup_prefix_keymap(hp, parent_prompt)
    p = PrefixHistoryPrompt(hp, parent_prompt)
    p.keymap_dict = keymap([prefix_history_keymap])
    pkeymap = AnyDict(
        "^P" => (s,o...)->(edit_move_up(s) || enter_prefix_search(s, p, true)),
        "^N" => (s,o...)->(edit_move_down(s) || enter_prefix_search(s, p, false)),
        # Up Arrow
        "\e[A" => (s,o...)->(edit_move_up(s) || enter_prefix_search(s, p, true)),
        # Down Arrow
        "\e[B" => (s,o...)->(edit_move_down(s) || enter_prefix_search(s, p, false)),
    )
    (p, pkeymap)
end

function deactivate(p::TextInterface, s::ModeState, termbuf, term::TextTerminal)
    clear_input_area(termbuf, s)
    s
end

function activate(p::TextInterface, s::ModeState, termbuf, term::TextTerminal)
    s.ias = InputAreaState(0, 0)
    refresh_line(s, termbuf)
end

function activate(p::TextInterface, s::MIState, termbuf, term::TextTerminal)
    @assert p == mode(s)
    activate(p, state(s), termbuf, term)
end
activate(m::ModalInterface, s::MIState, termbuf, term::TextTerminal) =
    activate(mode(s), s, termbuf, term)

commit_changes(t::UnixTerminal, termbuf) = write(t, take!(termbuf.out_stream))

function transition(f::Function, s::MIState, newmode)
    cancel_beep(s)
    if newmode === :abort
        s.aborted = true
        return
    end
    if newmode === :reset
        reset_state(s)
        return
    end
    if !haskey(s.mode_state, newmode)
        s.mode_state[newmode] = init_state(terminal(s), newmode)
    end
    termbuf = TerminalBuffer(IOBuffer())
    t = terminal(s)
    s.mode_state[mode(s)] = deactivate(mode(s), state(s), termbuf, t)
    s.current_mode = newmode
    f()
    activate(newmode, state(s, newmode), termbuf, t)
    commit_changes(t, termbuf)
end
transition(s::MIState, mode) = transition((args...)->nothing, s, mode)

function reset_state(s::PromptState)
    if s.input_buffer.size != 0
        s.input_buffer.size = 0
        s.input_buffer.ptr = 1
    end
    empty_undo(s)
    s.ias = InputAreaState(0, 0)
end

function reset_state(s::MIState)
    for (mode, state) in s.mode_state
        reset_state(state)
    end
end

const default_keymap_dict = keymap([default_keymap, escape_defaults])

function Prompt(prompt;
    prompt_prefix = "",
    prompt_suffix = "",
    keymap_dict = default_keymap_dict,
    repl = nothing,
    complete = EmptyCompletionProvider(),
    on_enter = default_enter_cb,
    on_done = ()->nothing,
    hist = EmptyHistoryProvider(),
    sticky = false)

    Prompt(prompt, prompt_prefix, prompt_suffix, keymap_dict, repl,
        complete, on_enter, on_done, hist, sticky)
end

run_interface(::Prompt) = nothing

init_state(terminal, prompt::Prompt) =
    PromptState(terminal, prompt, IOBuffer(), IOBuffer[], 1, InputAreaState(1, 1),
                #=indent(spaces)=# -1, Threads.SpinLock(), 0.0)

function init_state(terminal, m::ModalInterface)
    s = MIState(m, m.modes[1], false, Dict{Any,Any}())
    for mode in m.modes
        s.mode_state[mode] = init_state(terminal, mode)
    end
    s
end

function run_interface(terminal, m::ModalInterface)
    s::MIState = init_state(terminal, m)
    while !s.aborted
        buf, ok, suspend = prompt!(terminal, m, s)
        while suspend
            @static if Sys.isunix(); ccall(:jl_repl_raise_sigtstp, Cint, ()); end
            buf, ok, suspend = prompt!(terminal, m, s)
        end
        eval(Main,
            Expr(:body,
                Expr(:return,
                     Expr(:call,
                          QuoteNode(mode(state(s)).on_done),
                          QuoteNode(s),
                          QuoteNode(buf),
                          QuoteNode(ok)))))
    end
end

buffer(s::PromptState) = s.input_buffer
buffer(s::SearchState) = s.query_buffer
buffer(s::PrefixSearchState) = s.response_buffer
buffer(s::IOBuffer) = s

position(s::Union{MIState,ModeState}) = position(buffer(s))

function empty_undo(s::PromptState)
    empty!(s.undo_buffers)
    s.undo_idx = 1
end

empty_undo(s) = nothing

function push_undo(s::PromptState, advance=true)
    resize!(s.undo_buffers, s.undo_idx)
    s.undo_buffers[end] = copy(s.input_buffer)
    advance && (s.undo_idx += 1)
end

push_undo(s) = nothing

# must be called after a push_undo
function pop_undo(s::PromptState)
    pop!(s.undo_buffers)
    s.undo_idx -= 1
end

function edit_undo!(s::MIState)
    s.last_action ∉ (:edit_redo!, :edit_undo!) && push_undo(s, false)
    if edit_undo!(state(s))
        :edit_undo!
    else
        beep(s)
        :ignore
    end
end

function edit_undo!(s::PromptState)
    s.undo_idx > 1 || return false
    s.input_buffer = s.undo_buffers[s.undo_idx -=1]
    refresh_line(s)
    true
end
edit_undo!(s) = nothing

function edit_redo!(s::MIState)
    if s.last_action ∈ (:edit_redo!, :edit_undo!) && edit_redo!(state(s))
        :edit_redo!
    else
        beep(s)
        :ignore
    end
end

function edit_redo!(s::PromptState)
    s.undo_idx < length(s.undo_buffers) || return false
    s.input_buffer = s.undo_buffers[s.undo_idx += 1]
    refresh_line(s)
    true
end
edit_redo!(s) = nothing

keymap(s::PromptState, prompt::Prompt) = prompt.keymap_dict
keymap_data(s::PromptState, prompt::Prompt) = prompt.repl
keymap(ms::MIState, m::ModalInterface) = keymap(state(ms), mode(ms))
keymap_data(ms::MIState, m::ModalInterface) = keymap_data(state(ms), mode(ms))

function prompt!(term, prompt, s = init_state(term, prompt))
    Base.reseteof(term)
    raw!(term, true)
    enable_bracketed_paste(term)
    try
        activate(prompt, s, term, term)
        old_state = mode(s)
        while true
            kmap = keymap(s, prompt)
            fcn = match_input(kmap, s)
            kdata = keymap_data(s, prompt)
            local action
            # errors in keymaps shouldn't cause the REPL to fail, so wrap in a
            # try/catch block
            try
                action = fcn(s, kdata)
            catch e
                bt = catch_backtrace()
                warn(e, bt = bt, prefix = "ERROR (in the keymap): ")
                # try to cleanup and get `s` back to its original state before returning
                transition(s, :reset)
                transition(s, old_state)
                action = :done
            end
            action != :ignore && (s.last_action = action)
            if action === :abort
                return buffer(s), false, false
            elseif action === :done
                return buffer(s), true, false
            elseif action === :suspend
                if Sys.isunix()
                    return buffer(s), true, true
                end
            end
        end
    finally
        raw!(term, false) && disable_bracketed_paste(term)
    end
end


end # module
