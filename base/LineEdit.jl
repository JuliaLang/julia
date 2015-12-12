# This file is a part of Julia. License is MIT: http://julialang.org/license

module LineEdit

using ..Terminals

import ..Terminals: raw!, width, height, cmove, getX,
                       getY, clear_line, beep

import Base: ensureroom, peek, show, AnyDict

abstract TextInterface
abstract ModeState

export run_interface, Prompt, ModalInterface, transition, reset_state, edit_insert, keymap

immutable ModalInterface <: TextInterface
    modes
end

type MIState
    interface::ModalInterface
    current_mode
    aborted::Bool
    mode_state
    kill_buffer::String
    previous_key::Array{Char,1}
    key_repeats::Int
end
MIState(i, c, a, m) = MIState(i, c, a, m, "", Char[], 0)

function show(io::IO, s::MIState)
    print(io, "MI State (", s.current_mode, " active)")
end

type Prompt <: TextInterface
    prompt
    first_prompt
    # A string or function to be printed before the prompt. May not change the length of the prompt.
    # This may be used for changing the color, issuing other terminal escape codes, etc.
    prompt_prefix
    # Same as prefix except after the prompt
    prompt_suffix
    keymap_dict
    keymap_func_data
    complete
    on_enter
    on_done
    hist
    sticky::Bool
end

show(io::IO, x::Prompt) = show(io, string("Prompt(\"", x.prompt, "\",...)"))

immutable InputAreaState
    num_rows::Int64
    curs_row::Int64
end

type PromptState <: ModeState
    terminal
    p::Prompt
    input_buffer::IOBuffer
    ias::InputAreaState
    indent::Int
end

input_string(s::PromptState) = bytestring(s.input_buffer)

input_string_newlines(s::PromptState) = count(c->(c == '\n'), input_string(s))
function input_string_newlines_aftercursor(s::PromptState)
    str = input_string(s)
    isempty(str) && return 0
    rest = str[nextind(str, position(s.input_buffer)):end]
    return count(c->(c == '\n'), rest)
end

abstract HistoryProvider
abstract CompletionProvider

type EmptyCompletionProvider <: CompletionProvider
end

type EmptyHistoryProvider <: HistoryProvider
end

reset_state(::EmptyHistoryProvider) = nothing

complete_line(c::EmptyCompletionProvider, s) = [], true, true

terminal(s::IO) = s
terminal(s::PromptState) = s.terminal

for f in [:terminal, :edit_insert, :on_enter, :add_history, :buffer, :edit_backspace, :(Base.isempty),
        :replace_line, :refresh_multi_line, :input_string, :edit_move_left, :edit_move_right,
        :edit_move_word_left, :edit_move_word_right, :update_display_buffer]
    @eval ($f)(s::MIState, args...) = $(f)(s.mode_state[s.current_mode], args...)
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
        ret *= string(cc)
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
complete_line(s::MIState) = complete_line(s.mode_state[s.current_mode], s.key_repeats)
function complete_line(s::PromptState, repeats)
    completions, partial, should_complete = complete_line(s.p.complete, s)
    if isempty(completions)
        beep(terminal(s))
    elseif !should_complete
        # should_complete is false for cases where we only want to show
        # a list of possible completions but not complete, e.g. foo(\t
        show_completions(s, completions)
    elseif length(completions) == 1
        # Replace word by completion
        prev_pos = position(s.input_buffer)
        seek(s.input_buffer, prev_pos-sizeof(partial))
        edit_replace(s, position(s.input_buffer), prev_pos, completions[1])
    else
        p = common_prefix(completions)
        if !isempty(p) && p != partial
            # All possible completions share the same prefix, so we might as
            # well complete that
            prev_pos = position(s.input_buffer)
            seek(s.input_buffer, prev_pos-sizeof(partial))
            edit_replace(s, position(s.input_buffer), prev_pos, p)
        elseif repeats > 0
            show_completions(s, completions)
        end
    end
end

clear_input_area(terminal, s) = (_clear_input_area(terminal, s.ias); s.ias = InputAreaState(0, 0))
clear_input_area(s) = clear_input_area(s.terminal, s)
function _clear_input_area(terminal, state::InputAreaState)
    # Go to the last line
    if state.curs_row < state.num_rows
        cmove_down(terminal, state.num_rows-state.curs_row)
    end

    # Clear lines one by one going up
    for j=0:(state.num_rows-2)
        clear_line(terminal)
        cmove_up(terminal)
    end

    # Clear top line
    clear_line(terminal)
end

prompt_string(s::PromptState) = s.p.prompt
prompt_string(s::AbstractString) = s

refresh_multi_line(s::ModeState) = refresh_multi_line(terminal(s), s)
refresh_multi_line(termbuf::TerminalBuffer, s::ModeState) = refresh_multi_line(termbuf, terminal(s), s)
refresh_multi_line(termbuf::TerminalBuffer, term, s::ModeState) = (@assert term == terminal(s); refresh_multi_line(termbuf,s))
function refresh_multi_line(termbuf::TerminalBuffer, terminal::UnixTerminal, buf, state::InputAreaState, prompt = ""; indent = 0)
    cols = width(terminal)

    _clear_input_area(termbuf, state)

    curs_row = -1 # relative to prompt
    curs_col = -1 # absolute
    curs_pos = -1 # 1-based column position of the cursor
    cur_row = 0
    buf_pos = position(buf)
    line_pos = buf_pos
    # Write out the prompt string
    write_prompt(termbuf, prompt)
    prompt = prompt_string(prompt)

    seek(buf, 0)

    llength = 0

    l = ""

    plength = strwidth(prompt)
    pslength = length(prompt.data)
    # Now go through the buffer line by line
    while cur_row == 0 || (!isempty(l) && l[end] == '\n')
        l = readline(buf)
        hasnl = !isempty(l) && l[end] == '\n'
        cur_row += 1
        # We need to deal with UTF8 characters. Since the IOBuffer is a bytearray, we just count bytes
        llength = strwidth(l)
        slength = length(l.data)
        if cur_row == 1 # First line
            if line_pos < slength
                num_chars = strwidth(l[1:line_pos])
                curs_row = div(plength+num_chars-1, cols) + 1
                curs_pos = (plength+num_chars-1) % cols + 1
            end
            # Substract -1 if there's a '\n' at the end of the line (since it doesn't take up a column)
            # The other -1, since we want 10,20 for cols=10 to still not add a row (but we want 11,21 to)
            cur_row += div(max(plength+(llength-hasnl)-1,0), cols)
            line_pos -= slength
            write(termbuf, l)
        else
            # We expect to be line after the last valid output line (due to
            # the '\n' at the end of the previous line)
            if curs_row == -1
                if line_pos < slength
                    num_chars = strwidth(l[1:line_pos])
                    curs_row = cur_row + div(indent+num_chars-1, cols)
                    curs_pos = (indent+num_chars-1) % cols + 1
                end
                line_pos -= slength # '\n' gets an extra pos
                cur_row += div(max(indent+(llength-hasnl)-1,0), cols)
                cmove_col(termbuf, indent+1)
                write(termbuf, l)
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
            else
                cur_row += div(max(indent+(llength-hasnl)-1,0), cols)
                cmove_col(termbuf, indent+1)
                write(termbuf, l)
            end
        end
    end

    seek(buf, buf_pos)

    # If we are at the end of the buffer, we need to put the cursor one past the
    # last character we have written

    if curs_row == -1
        curs_pos = ((cur_row == 1 ? plength : indent)+llength-1) % cols + 1
        curs_row = cur_row
    end

    # Same issue as above. TODO: We should figure out
    # how to refactor this to avoid duplicating functionality.
    if curs_pos == cols
        if line_pos == 0
            write(termbuf, "\n")
            cur_row += 1
        end
        curs_row += 1
        curs_pos = 0
        cmove_col(termbuf, 1)
    end

    # Let's move the cursor to the right position
    # The line first
    n = cur_row - curs_row
    if n > 0
        cmove_up(termbuf, n)
    end

    #columns are 1 based
    cmove_col(termbuf, curs_pos+1)

    # Updated cur_row,curs_row
    return InputAreaState(cur_row, curs_row)
end

function refresh_multi_line(terminal::UnixTerminal, args...; kwargs...)
    outbuf = IOBuffer()
    termbuf = TerminalBuffer(outbuf)
    ret = refresh_multi_line(termbuf, terminal, args...;kwargs...)
    # Output the entire refresh at once
    write(terminal, takebuf_array(outbuf))
    flush(terminal)
    return ret
end


# Edit functionality
is_non_word_char(c) = c in " \t\n\"\\'`@\$><=:;|&{}()[].,+-*/?%^~"

function reset_key_repeats(f::Function, s::MIState)
    key_repeats_sav = s.key_repeats
    try
        s.key_repeats = 0
        f()
    finally
        s.key_repeats = key_repeats_sav
    end
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
            if charwidth(c) != 0 || c == '\n' || position(buf) == 0
                break
            end
        end
        return true
    end
    return false
end
edit_move_left(s::PromptState) = edit_move_left(s.input_buffer) && refresh_line(s)

function edit_move_word_left(s)
    if position(s.input_buffer) > 0
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
            (charwidth(nextc) != 0 || nextc == '\n') && break
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
    offset = length(bytestring(buf.data[(npos+1):(position(buf))]))
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
    offset = length(bytestring(buf.data[(npos+1):(position(buf))]))
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

# splice! for IOBuffer: convert from 0-indexed positions, update the size,
# and keep the cursor position stable with the text
function splice_buffer!{T<:Integer}(buf::IOBuffer, r::UnitRange{T}, ins::AbstractString = "")
    pos = position(buf)
    if !isempty(r) && pos in r
        seek(buf, first(r))
    elseif pos > last(r)
        seek(buf, pos - length(r))
    end
    splice!(buf.data, r .+ 1, ins.data) # position(), etc, are 0-indexed
    buf.size = buf.size + sizeof(ins) - length(r)
    seek(buf, position(buf) + sizeof(ins))
end

function edit_replace(s, from, to, str)
    splice_buffer!(buffer(s), from:to-1, str)
end

function edit_insert(s::PromptState, c)
    str = string(c)
    edit_insert(s.input_buffer, str)
    if !('\n' in str) && eof(s.input_buffer) &&
        ((position(s.input_buffer) + length(s.p.prompt) + sizeof(str) - 1) < width(terminal(s)))
        #Avoid full update
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
        splice_buffer!(buf, position(buf):position(buf)-1, s)
        return sizeof(s)
    end
end

function edit_backspace(s::PromptState)
    if edit_backspace(s.input_buffer)
        refresh_line(s)
    else
        beep(terminal(s))
    end
end
function edit_backspace(buf::IOBuffer)
    if position(buf) > 0
        oldpos = position(buf)
        char_move_left(buf)
        splice_buffer!(buf, position(buf):oldpos-1)
        return true
    else
        return false
    end
end

edit_delete(s) = edit_delete(buffer(s)) ? refresh_line(s) : beep(terminal(s))
function edit_delete(buf::IOBuffer)
    eof(buf) && return false
    oldpos = position(buf)
    char_move_right(buf)
    splice_buffer!(buf, oldpos:position(buf)-1)
    true
end

function edit_werase(buf::IOBuffer)
    pos1 = position(buf)
    char_move_word_left(buf, isspace)
    pos0 = position(buf)
    pos0 < pos1 || return false
    splice_buffer!(buf, pos0:pos1-1)
    true
end
function edit_werase(s)
    edit_werase(buffer(s)) && refresh_line(s)
end

function edit_delete_prev_word(buf::IOBuffer)
    pos1 = position(buf)
    char_move_word_left(buf)
    pos0 = position(buf)
    pos0 < pos1 || return false
    splice_buffer!(buf, pos0:pos1-1)
    true
end
function edit_delete_prev_word(s)
    edit_delete_prev_word(buffer(s)) && refresh_line(s)
end

function edit_delete_next_word(buf::IOBuffer)
    pos0 = position(buf)
    char_move_word_right(buf)
    pos1 = position(buf)
    pos0 < pos1 || return false
    splice_buffer!(buf, pos0:pos1-1)
    true
end
function edit_delete_next_word(s)
    edit_delete_next_word(buffer(s)) && refresh_line(s)
end

function edit_yank(s::MIState)
    edit_insert(buffer(s), s.kill_buffer)
    refresh_line(s)
end

function edit_kill_line(s::MIState)
    buf = buffer(s)
    pos = position(buf)
    killbuf = readline(buf)
    if length(killbuf) > 1 && killbuf[end] == '\n'
        killbuf = killbuf[1:end-1]
        char_move_left(buf)
    end
    s.kill_buffer = s.key_repeats > 0 ? s.kill_buffer * killbuf : killbuf

    splice_buffer!(buf, pos:position(buf)-1)
    refresh_line(s)
end

edit_transpose(s) = edit_transpose(buffer(s)) && refresh_line(s)
function edit_transpose(buf::IOBuffer)
    position(buf) == 0 && return false
    eof(buf) && char_move_left(buf)
    char_move_left(buf)
    pos = position(buf)
    a, b = read(buf, Char), read(buf, Char)
    seek(buf, pos)
    write(buf, b, a)
    return true
end

edit_clear(buf::IOBuffer) = truncate(buf, 0)

function edit_clear(s::MIState)
    edit_clear(buffer(s))
    refresh_line(s)
end

function replace_line(s::PromptState, l::IOBuffer)
    s.input_buffer = copy(l)
end

function replace_line(s::PromptState, l)
    s.input_buffer.ptr = 1
    s.input_buffer.size = 0
    write(s.input_buffer, l)
end

history_prev(::EmptyHistoryProvider) = ("", false)
history_next(::EmptyHistoryProvider) = ("", false)
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
        beep(terminal(s))
    end
end
function history_next(s, hist)
    l, ok = history_next(mode(s).hist)
    if ok
        replace_line(s, l)
        move_input_end(s)
        refresh_line(s)
    else
        beep(terminal(s))
    end
end

refresh_line(s) = refresh_multi_line(s)
refresh_line(s, termbuf) = refresh_multi_line(termbuf, s)

default_completion_cb(::IOBuffer) = []
default_enter_cb(_) = true

write_prompt(terminal, s::PromptState) = write_prompt(terminal, s.p)
function write_prompt(terminal, p::Prompt)
    prefix = isa(p.prompt_prefix,Function) ? p.prompt_prefix() : p.prompt_prefix
    suffix = isa(p.prompt_suffix,Function) ? p.prompt_suffix() : p.prompt_suffix
    write(terminal, prefix)
    write(terminal, p.prompt)
    write(terminal, Base.text_colors[:normal])
    write(terminal, suffix)
end
write_prompt(terminal, s::String) = write(terminal, s)

### Keymap Support

normalize_key(key::Char) = string(key)
normalize_key(key::Integer) = normalize_key(Char(key))
function normalize_key(key::AbstractString)
    '\0' in key && error("Matching \\0 not currently supported.")
    buf = IOBuffer()
    i = start(key)
    while !done(key, i)
        c, i = next(key, i)
        if c == '*'
            write(buf, '\0')
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
    return takebuf_string(buf)
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
        if c in keys(keymap)
            if done(key, i) && override
                # isa(keymap[c], Dict) - In this case we're overriding a prefix of an existing command
                keymap[c] = value
                break
            else
                if !isa(keymap[c], Dict)
                    error("Conflicting definitions for keyseq " * escape_string(key) * " within one keymap")
                end
            end
        elseif done(key, i)
            keymap[c] = value
            break
        else
            keymap[c] = Dict{Char,Any}()
        end
        keymap = keymap[c]
    end
end

# Redirect a key as if `seq` had been the keysequence instead in a lazy fashion.
# This is different from the default eager redirect, which only looks at the current and lower
# layers of the stack.
immutable KeyAlias
    seq::String
    KeyAlias(seq) = new(normalize_key(seq))
end

match_input(k::Function, s, term, cs, keymap) = (update_key_repeats(s, cs); return keymap_fcn(k, String(cs)))
match_input(k::Void, s, term, cs, keymap) = (s,p) -> return :ok
match_input(k::KeyAlias, s, term, cs, keymap) = match_input(keymap, s, IOBuffer(k.seq), Char[], keymap)
function match_input(k::Dict, s, term=terminal(s), cs=Char[], keymap = k)
    # if we run out of characters to match before resolving an action,
    # return an empty keymap function
    eof(term) && return keymap_fcn(nothing, "")
    c = read(term, Char)
    push!(cs, c)
    key = haskey(k, c) ? c : '\0'
    # if we don't match on the key, look for a default action then fallback on 'nothing' to ignore
    return match_input(get(k, key, nothing), s, term, cs, keymap)
end

keymap_fcn(f::Void, c) = (s, p) -> return :ok
function keymap_fcn(f::Function, c)
    return function (s, p)
        r = f(s, p, c)
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
    default_branch = subdict['\0']
    if isa(default_branch, Dict)
        # Go through all the keymaps in the default branch
        # and copy them over to dict
        for s in keys(default_branch)
            s == '\0' && add_specialisations(dict, default_branch, level+1)
            fixup_keymaps!(dict, level, s, default_branch[s])
        end
    end
end

postprocess!(others) = nothing
function postprocess!(dict::Dict)
    # needs to be done first for every branch
    if haskey(dict, '\0')
        add_specialisations(dict, dict, 1)
    end
    for (k,v) in dict
        k == '\0' && continue
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
    direct_keys = filter((k,v) -> isa(v, Union{Function, KeyAlias, Void}), source)
    # first direct entries
    for key in keys(direct_keys)
        add_nested_key!(ret, key, source[key]; override = true)
    end
    # then redirected entries
    for key in setdiff(keys(source), keys(direct_keys))
        # We first resolve redirects in the source
        value = source[key]
        visited = Array(Any,0)
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

function keymap{D<:Dict}(keymaps::Array{D})
    # keymaps is a vector of prioritized keymaps, with highest priority first
    ret = keymap_unify(map(normalize_keys, reverse(keymaps)))
    validate_keymap(ret)
    ret
end

const escape_defaults = merge!(
    AnyDict([Char(i) => nothing for i=vcat(1:26, 28:31)]), # Ignore control characters by default
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
    AnyDict(["\e[$(c)h" => nothing for c in 1:20]),
    # reset mode commands
    AnyDict(["\e[$(c)l" => nothing for c in 1:20])
    )

function write_response_buffer(s::PromptState, data)
    offset = s.input_buffer.ptr
    ptr = data.response_buffer.ptr
    seek(data.response_buffer, 0)
    write(s.input_buffer, readstring(data.response_buffer))
    s.input_buffer.ptr = offset + ptr - 2
    data.response_buffer.ptr = ptr
    refresh_line(s)
end

type SearchState <: ModeState
    terminal
    histprompt
    #rsearch (true) or ssearch (false)
    backward::Bool
    query_buffer::IOBuffer
    response_buffer::IOBuffer
    ias::InputAreaState
    #The prompt whose input will be replaced by the matched history
    parent
    SearchState(terminal, histprompt, backward, query_buffer, response_buffer) =
        new(terminal, histprompt, backward, query_buffer, response_buffer, InputAreaState(0,0))
end

terminal(s::SearchState) = s.terminal

function update_display_buffer(s::SearchState, data)
    history_search(data.histprompt.hp, data.query_buffer, data.response_buffer, data.backward, false) || beep(terminal(s))
    refresh_line(s)
end

function history_next_result(s::MIState, data::SearchState)
    history_search(data.histprompt.hp, data.query_buffer, data.response_buffer, data.backward, true) || beep(terminal(s))
    refresh_line(data)
end

function history_set_backward(s::SearchState, backward)
    s.backward = backward
end

input_string(s::SearchState) = bytestring(s.query_buffer)

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

type HistoryPrompt{T<:HistoryProvider} <: TextInterface
    hp::T
    complete
    keymap_dict::Dict{Char,Any}
    HistoryPrompt(hp) = new(hp, EmptyCompletionProvider())
end

HistoryPrompt{T<:HistoryProvider}(hp::T) = HistoryPrompt{T}(hp)
init_state(terminal, p::HistoryPrompt) = SearchState(terminal, p, true, IOBuffer(), IOBuffer())

type PrefixSearchState <: ModeState
    terminal
    histprompt
    prefix::String
    response_buffer::IOBuffer
    ias::InputAreaState
    indent::Int
    # The modal interface state, if present
    mi
    #The prompt whose input will be replaced by the matched history
    parent
    PrefixSearchState(terminal, histprompt, prefix, response_buffer) =
        new(terminal, histprompt, prefix, response_buffer, InputAreaState(0,0), 0)
end

function show(io::IO, s::PrefixSearchState)
    print(io, "PrefixSearchState ", isdefined(s,:parent) ?
     string("(", s.parent, " active)") : "(no parent)", " for ",
     isdefined(s,:mi) ? s.mi : "no MI")
end

refresh_multi_line(termbuf::TerminalBuffer, terminal::UnixTerminal,
    s::Union{PromptState,PrefixSearchState}) = s.ias =
    refresh_multi_line(termbuf, terminal, buffer(s), s.ias, s, indent = s.indent)

input_string(s::PrefixSearchState) = bytestring(s.response_buffer)

# a meta-prompt that presents itself as parent_prompt, but which has an independent keymap
# for prefix searching
type PrefixHistoryPrompt{T<:HistoryProvider} <: TextInterface
    hp::T
    parent_prompt::Prompt
    complete
    keymap_dict::Dict{Char,Any}
    PrefixHistoryPrompt(hp, parent_prompt) = new(hp, parent_prompt, EmptyCompletionProvider())
end

PrefixHistoryPrompt{T<:HistoryProvider}(hp::T, parent_prompt) = PrefixHistoryPrompt{T}(hp, parent_prompt)
init_state(terminal, p::PrefixHistoryPrompt) = PrefixSearchState(terminal, p, "", IOBuffer())

write_prompt(terminal, s::PrefixSearchState) = write_prompt(terminal, s.histprompt.parent_prompt)
prompt_string(s::PrefixSearchState) = s.histprompt.parent_prompt.prompt

terminal(s::PrefixSearchState) = s.terminal

function reset_state(s::PrefixSearchState)
    if s.response_buffer.size != 0
        s.response_buffer.size = 0
        s.response_buffer.ptr = 1
    end
    reset_state(s.histprompt.hp)
end

function transition(f::Function, s::PrefixSearchState, mode)
    if isdefined(s,:mi)
        transition(f,s.mi,mode)
    end
    s.parent = mode
    s.histprompt.parent_prompt = mode
    if isdefined(s,:mi)
        transition(f,s.mi,s.histprompt)
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
    write(buf, readstring(s.response_buffer))
    buf.ptr = offset + ptr - 1
    s.response_buffer.ptr = ptr
    s.ias = refresh_multi_line(termbuf, s.terminal, buf, s.ias, s.backward ? "(reverse-i-search)`" : "(forward-i-search)`")
end

state(s::MIState, p) = s.mode_state[p]
state(s::PromptState, p) = (@assert s.p == p; s)
mode(s::MIState) = s.current_mode
mode(s::PromptState) = s.p
mode(s::SearchState) = @assert false
mode(s::PrefixSearchState) = s.histprompt.parent_prompt

# Search Mode completions
function complete_line(s::SearchState, repeats)
    completions, partial, should_complete = complete_line(s.histprompt.complete, s)
    # For now only allow exact completions in search mode
    if length(completions) == 1
        prev_pos = position(s.query_buffer)
        seek(s.query_buffer, prev_pos-sizeof(partial))
        edit_replace(s, position(s.query_buffer), prev_pos, completions[1])
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
        pss.prefix = bytestring(pointer(buf.data), position(buf))
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
                        update_display_buffer(s, data) : beep(terminal(s))),
        127       => KeyAlias('\b'),
        # Meta Backspace
        "\e\b"    => (s,data,c)->(edit_delete_prev_word(data.query_buffer) ?
                        update_display_buffer(s, data) : beep(terminal(s))),
        "\e\x7f"  => "\e\b",
        # Word erase to whitespace
        "^W"      => (s,data,c)->(edit_werase(data.query_buffer) ?
                        update_display_buffer(s, data) : beep(terminal(s))),
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
            input = readuntil(ps.terminal, "\e[201~")[1:(end-6)]
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
end
function move_line_end(s::MIState)
    s.key_repeats > 0 ?
        move_input_end(s) :
        move_line_end(buffer(s))
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
    move_input_end(s)
    refresh_line(s)
    println(terminal(s))
    add_history(s)
    state(s, mode(s)).ias = InputAreaState(0, 0)
end

"""
`Base.LineEdit.tabwidth` controls the presumed tab width of code pasted into the REPL.

You can modify it by doing `eval(Base.LineEdit, :(tabwidth = 4))`, for example.

Must satisfy `0 < tabwidth <= 16`.
"""
global tabwidth = 8

function bracketed_paste(s)
    ps = state(s, mode(s))
    input = readuntil(ps.terminal, "\e[201~")[1:(end-6)]
    input = replace(input, '\r', '\n')
    if position(buffer(s)) == 0
        indent = Base.indentation(input; tabwidth=tabwidth)[1]
        input = Base.unindent(input[(indent+1):end], indent; tabwidth=tabwidth)
    end
    input
end

const default_keymap =
AnyDict(
    # Tab
    '\t' => (s,o...)->begin
        buf = buffer(s)
        # Yes, we are ignoring the possiblity
        # the we could be in the middle of a multi-byte
        # sequence, here but that's ok, since any
        # whitespace we're interested in is only one byte
        i = position(buf)
        if i != 0
            c = buf.data[i]
            if c == '\n' || c == '\t' ||
               # hack to allow path completion in cmds
               # after a space, e.g., `cd <tab>`, while still
               # allowing multiple indent levels
               (c == ' ' && i > 3 && buf.data[i-1] == ' ')
                edit_insert(s, " "^4)
                return
            end
        end
        complete_line(s)
        refresh_line(s)
    end,
    # Enter
    '\r' => (s,o...)->begin
        if on_enter(s) || (eof(buffer(s)) && s.key_repeats > 1)
            commit_line(s)
            return :done
        else
            edit_insert(s, '\n')
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
    "^B" => (s,o...)->edit_move_left(s),
    "^F" => (s,o...)->edit_move_right(s),
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
    "\e\r" => (s,o...)->(edit_insert(s, '\n')),
    "\e\n" => "\e\r",
    # Simply insert it into the buffer by default
    "*" => (s,data,c)->(edit_insert(s, c)),
    "^U" => (s,o...)->edit_clear(s),
    "^K" => (s,o...)->edit_kill_line(s),
    "^Y" => (s,o...)->edit_yank(s),
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
    "^T" => (s,o...)->edit_transpose(s)
)

const history_keymap = AnyDict(
    "^P" => (s,o...)->(history_prev(s, mode(s).hist)),
    "^N" => (s,o...)->(history_next(s, mode(s).hist)),
    # Up Arrow
    "\e[A" => (s,o...)->(edit_move_up(s) || history_prev(s, mode(s).hist)),
    # Down Arrow
    "\e[B" => (s,o...)->(edit_move_down(s) || history_next(s, mode(s).hist)),
    # Page Up
    "\e[5~" => (s,o...)->(history_prev(s, mode(s).hist)),
    # Page Down
    "\e[6~" => (s,o...)->(history_next(s, mode(s).hist))
)

const prefix_history_keymap = merge!(
    AnyDict(
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
    AnyDict(["\e[$(n)~" => "*" for n in 1:8]),
    # set mode commands
    AnyDict(["\e[$(c)h" => "*" for c in 1:20]),
    # reset mode commands
    AnyDict(["\e[$(c)l" => "*" for c in 1:20])
)

function setup_prefix_keymap(hp, parent_prompt)
    p = PrefixHistoryPrompt(hp, parent_prompt)
    p.keymap_dict = keymap([prefix_history_keymap])
    pkeymap = AnyDict(
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
    @assert p == s.current_mode
    activate(p, s.mode_state[s.current_mode], termbuf, term)
end
activate(m::ModalInterface, s::MIState, termbuf, term::TextTerminal) =
    activate(s.current_mode, s, termbuf, term)

commit_changes(t::UnixTerminal, termbuf) = write(t, takebuf_array(termbuf.out_stream))
function transition(f::Function, s::MIState, mode)
    if mode == :abort
        s.aborted = true
        return
    end
    if mode == :reset
        reset_state(s)
        return
    end
    if !haskey(s.mode_state,mode)
        s.mode_state[mode] = init_state(terminal(s), mode)
    end
    termbuf = TerminalBuffer(IOBuffer())
    t = terminal(s)
    s.mode_state[s.current_mode] = deactivate(s.current_mode, s.mode_state[s.current_mode], termbuf, t)
    s.current_mode = mode
    f()
    activate(mode, s.mode_state[mode], termbuf, t)
    commit_changes(t, termbuf)
end
transition(s::MIState, mode) = transition((args...)->nothing, s, mode)

function reset_state(s::PromptState)
    if s.input_buffer.size != 0
        s.input_buffer.size = 0
        s.input_buffer.ptr = 1
    end
    s.ias = InputAreaState(0, 0)
end

function reset_state(s::MIState)
    for (mode,state) in s.mode_state
        reset_state(state)
    end
end

const default_keymap_dict = keymap([default_keymap, escape_defaults])

function Prompt(prompt;
    first_prompt = prompt,
    prompt_prefix = "",
    prompt_suffix = "",
    keymap_dict = default_keymap_dict,
    keymap_func_data = nothing,
    complete = EmptyCompletionProvider(),
    on_enter = default_enter_cb,
    on_done = ()->nothing,
    hist = EmptyHistoryProvider(),
    sticky = false)

    Prompt(prompt, first_prompt, prompt_prefix, prompt_suffix, keymap_dict, keymap_func_data,
        complete, on_enter, on_done, hist, sticky)
end

run_interface(::Prompt) = nothing

init_state(terminal, prompt::Prompt) = PromptState(terminal, prompt, IOBuffer(), InputAreaState(1, 1), length(prompt.prompt))

function init_state(terminal, m::ModalInterface)
    s = MIState(m, m.modes[1], false, Dict{Any,Any}())
    for mode in m.modes
        s.mode_state[mode] = init_state(terminal, mode)
    end
    s
end

function run_interface(terminal, m::ModalInterface)
    s = init_state(terminal, m)
    while !s.aborted
        p = s.current_mode
        buf, ok, suspend = prompt!(terminal, m, s)
        while suspend
            @unix_only ccall(:jl_repl_raise_sigtstp, Cint, ())
            buf, ok, suspend = prompt!(terminal, m, s)
        end
        s.mode_state[s.current_mode].p.on_done(s, buf, ok)
    end
end

buffer(s::PromptState) = s.input_buffer
buffer(s::SearchState) = s.query_buffer
buffer(s::PrefixSearchState) = s.response_buffer

keymap(s::PromptState, prompt::Prompt) = prompt.keymap_dict
keymap_data(s::PromptState, prompt::Prompt) = prompt.keymap_func_data
keymap(ms::MIState, m::ModalInterface) = keymap(ms.mode_state[ms.current_mode], ms.current_mode)
keymap_data(ms::MIState, m::ModalInterface) = keymap_data(ms.mode_state[ms.current_mode], ms.current_mode)

function prompt!(term, prompt, s = init_state(term, prompt))
    Base.reseteof(term)
    raw!(term, true)
    enable_bracketed_paste(term)
    try
        activate(prompt, s, term, term)
        while true
            map = keymap(s, prompt)
            fcn = match_input(map, s)
            # errors in keymaps shouldn't cause the REPL to fail, so wrap in a
            # try/catch block
            local state
            try
                state = fcn(s, keymap_data(s, prompt))
            catch e
                warn("Caught an exception in the keymap:")
                warn(e)
                state = :done
            end
            if state == :abort
                return buffer(s), false, false
            elseif state == :done
                return buffer(s), true, false
            elseif state == :suspend
                @unix_only begin
                    return buffer(s), true, true
                end
            else
                @assert state == :ok
            end
        end
    finally
        raw!(term, false) && disable_bracketed_paste(term)
    end
end

end # module
