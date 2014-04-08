module LineEdit

using Base.Terminals

import Base.Terminals: raw!, width, height, cmove, getX,
                       getY, clear_line, beep

import Base: ensureroom, peek, show

abstract TextInterface

export run_interface, Prompt, ModalInterface, transition, reset_state, edit_insert

immutable ModalInterface <: TextInterface
    modes
end

type MIState
    interface::ModalInterface
    current_mode
    aborted::Bool
    mode_state
    kill_buffer::ByteString
end

type Mode <: TextInterface
end

type Prompt <: TextInterface
    prompt
    first_prompt
    prompt_color::ASCIIString
    keymap_func
    keymap_func_data
    input_color
    complete
    on_enter
    on_done
    hist
end

show(io::IO, x::Prompt) = show(io, string("Prompt(\"", x.prompt, "\",...)"))

immutable InputAreaState
    num_rows::Int64
    curs_row::Int64
end

type PromptState
    terminal::TextTerminal
    p::Prompt
    input_buffer::IOBuffer
    ias::InputAreaState
    indent::Int
end

input_string(s::PromptState) = bytestring(pointer(s.input_buffer.data), s.input_buffer.size)

input_string_newlines(s::PromptState) = count(c->(c == '\n'), input_string(s))
function input_string_newlines_aftercursor(s::PromptState)
    str = input_string(s)
    length(str) == 0 && return 0
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

complete_line(c::EmptyCompletionProvider, s) = []

terminal(s::IO) = s
terminal(s::PromptState) = s.terminal

for f in [:terminal, :edit_insert, :on_enter, :add_history, :buffer, :edit_backspace, :(Base.isempty),
        :replace_line, :refresh_multi_line, :input_string, :complete_line, :edit_move_left, :edit_move_right,
        :edit_move_word_left, :edit_move_word_right, :update_display_buffer]
    @eval ($f)(s::MIState,args...) = $(f)(s.mode_state[s.current_mode], args...)
end

function common_prefix(completions)
    ret = ""
    i = nexti = 1
    cc, nexti = next(completions[1], 1)
    while true
        for c in completions
            (i > length(c) || c[i] != cc) && return ret
        end
        ret *= string(cc)
        i >= length(completions[1]) && return ret
        i = nexti
        cc, nexti = next(completions[1], i)
    end
end

# Show available completions
function show_completions(s::PromptState, completions)
    colmax = maximum(map(length, completions))
    num_cols = max(div(width(LineEdit.terminal(s)), colmax+2), 1)
    entries_per_col, r = divrem(length(completions), num_cols)
    entries_per_col += r != 0
    # skip any lines of input after the cursor
    cmove_down(LineEdit.terminal(s), input_string_newlines_aftercursor(s))
    println(LineEdit.terminal(s))
    for row = 1:entries_per_col
        for col = 0:num_cols
            idx = row + col*entries_per_col
            if idx <= length(completions)
                cmove_col(LineEdit.terminal(s), (colmax+2)*col)
                print(LineEdit.terminal(s), completions[idx])
            end
        end
        println(LineEdit.terminal(s))
    end
    # make space for the prompt
    for i = 1:input_string_newlines(s)
        println(LineEdit.terminal(s))
    end
end

function complete_line(s::PromptState)
    completions, partial, should_complete = complete_line(s.p.complete, s)
    if length(completions) == 0
        beep(LineEdit.terminal(s))
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
        if length(p) > 0 && p != partial
            # All possible completions share the same prefix, so we might as
            # well complete that
            prev_pos = position(s.input_buffer)
            seek(s.input_buffer, prev_pos-sizeof(partial))
            edit_replace(s, position(s.input_buffer), prev_pos,p)
        else
            show_completions(s, completions)
        end
    end
end

clear_input_area(terminal, s) = (_clear_input_area(terminal, s.ias); s.ias = InputAreaState(0, 0))
clear_input_area(s) = clear_input_area(s.terminal,s)
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
prompt_string(s::String) = s

refresh_multi_line(termbuf::TerminalBuffer,s::PromptState) = s.ias = 
    refresh_multi_line(termbuf,terminal(s), buffer(s), s.ias, s, indent = s.indent)

function refresh_multi_line(termbuf::TerminalBuffer, terminal::TTYTerminal, buf, state::InputAreaState,prompt = ""; indent = 0)
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

    plength = length(prompt)
    pslength = length(prompt.data)
    # Now go through the buffer line by line
    while cur_row == 0 || (!isempty(l) && l[end] == '\n')
        l = readline(buf)
        cur_row += 1
        # We need to deal with UTF8 characters. Since the IOBuffer is a bytearray, we just count bytes
        llength = length(l)
        slength = length(l.data)
        if cur_row == 1 #First line
            if line_pos < slength
                num_chars = length(l[1:line_pos])
                curs_row = div(plength+num_chars-1, cols) + 1
                curs_pos = (plength+num_chars-1) % cols + 1
            end
            # One -1 for the '\n' at the end of the line (since it doesn't take up a column)
            # The other -1, since we want 10,20 for cols=10 to still not add a row (but we want 11,21 to)
            cur_row += div(max(plength+(llength-1)-1,0), cols)
            line_pos -= slength
            write(termbuf, l)
        else
            # We expect to be line after the last valid output line (due to
            # the '\n' at the end of the previous line)
            if curs_row == -1
                if line_pos < slength
                    num_chars = length(l[1:line_pos])
                    curs_row = cur_row + div(indent+num_chars-1, cols)
                    curs_pos = (indent+num_chars-1) % cols + 1
                end
                line_pos -= slength #'\n' gets an extra pos
                cur_row += div(max(indent+(llength-1)-1,0), cols)
                cmove_col(termbuf, indent+1)
                write(termbuf, l)
                # There's an issue if the last character we wrote was at the very right end of the screen. In that case we need to
                # emit a new line and move the cursor there.
                if curs_pos == cols
                    write(termbuf, "\n")
                    cmove_col(termbuf, 1)
                    curs_row += 1
                    curs_pos = 0
                    cur_row += 1
                end
            else
                cur_row += div(llength+indent-1, cols)
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
    # how to refactor this to avoid duplcating functionality.
    if curs_pos == cols
        write(termbuf, "\n")
        cmove_col(termbuf, 1)
        curs_row += 1
        curs_pos = 0
        cur_row += 1
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


# Edit functionality
is_non_word_char(c) = in(c," \t\n\"\\'`@\$><=:;|&{}()[].,+-*/?%^~")

char_move_left(s::PromptState) = char_move_left(s.input_buffer)
function char_move_left(buf::IOBuffer)
    while position(buf) > 0
        seek(buf, position(buf)-1)
        c = peek(buf)
        (((c & 0x80) == 0) || ((c & 0xc0) == 0xc0)) && break
    end
    pos = position(buf)
    c = read(buf,Char)
    seek(buf,pos)
    c
end

function edit_move_left(s::PromptState)
    if position(s.input_buffer) > 0
        #move to the next UTF8 character to the left
        char_move_left(s.input_buffer)
        refresh_line(s)
    end
end

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
            seek(buf,pos)
            break
        end
    end
end

function char_move_word_left(buf::IOBuffer,is_delimiter=is_non_word_char)
    while position(buf) > 0 && is_delimiter(char_move_left(buf))
    end
    while position(buf) > 0
        pos = position(buf)
        if is_delimiter(char_move_left(buf))
            seek(buf,pos)
            break
        end
    end
end

char_move_word_right(s) = char_move_word_right(buffer(s))
char_move_word_left(s) = char_move_word_left(buffer(s))

function edit_move_right(s)
    if position(s.input_buffer) != s.input_buffer.size
        # move to the next UTF8 character to the right
        char_move_right(s)
        refresh_line(s)
    end
end

function edit_move_word_right(s)
    if position(s.input_buffer) != s.input_buffer.size
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

function memmove(dst::IOBuffer, idst::Int, src::IOBuffer, isrc::Int, num::Int)
    num == 0 && return
    @assert 0 < num
    @assert 0 < idst <= length(dst.data) - num + 1
    @assert 0 < isrc <= length(src.data) - num + 1
    pdst = pointer(dst.data, idst)
    psrc = pointer(src.data, isrc)
    ccall(:memmove, Void, (Ptr{Void},Ptr{Void},Csize_t), pdst, psrc, num)
end

function edit_replace(s, from, to, str)
    room = length(str.data) - (to - from)
    ensureroom(s.input_buffer, s.input_buffer.size + room)
    memmove(s.input_buffer, to+room+1, s.input_buffer, to+1, s.input_buffer.size-to)
    s.input_buffer.size += room
    seek(s.input_buffer, from)
    write(s.input_buffer, str)
end

function edit_insert(s::PromptState, c)
    str = string(c)
    edit_insert(s.input_buffer, str)
    if !('\n' in str) && eof(s.input_buffer) &&
        ((position(s.input_buffer) + length(s.p.prompt) + sizeof(str) - 1) < width(LineEdit.terminal(s)))
        #Avoid full update
        write(LineEdit.terminal(s), str)
    else
        refresh_line(s)
    end
end

# TODO: Don't use memmove
function edit_insert(buf::IOBuffer, c)
    if eof(buf)
        write(buf, c)
    else
        s = string(c)
        ensureroom(buf, buf.size-position(buf)+sizeof(s))
        oldpos = position(buf)
        memmove(buf, position(buf)+1+sizeof(s), buf, position(buf)+1, buf.size-position(buf))
        buf.size += sizeof(s)
        write(buf, c)
    end
end

function edit_backspace(s::PromptState)
    if edit_backspace(s.input_buffer)
        refresh_line(s)
    else
        beep(LineEdit.terminal(s))
    end
end
function edit_backspace(buf::IOBuffer)
    if position(buf) > 0 && buf.size > 0
        oldpos = position(buf)
        char_move_left(buf)
        memmove(buf, position(buf)+1, buf, oldpos+1, buf.size-oldpos)
        buf.size -= oldpos-position(buf)
        return true
    else
        return false
    end
end

edit_delete(s) = edit_delete(buffer(s)) ? refresh_line(s) : beep(LineEdit.terminal(s))
function edit_delete(buf::IOBuffer)
    # (buf.size == 0 || eof(buf)) && return false
    if buf.size > 0 && position(buf) < buf.size
        oldpos = position(buf)
        char_move_right(buf)
        memmove(buf, oldpos+1, buf, position(buf)+1, buf.size-position(buf))
        buf.size -= position(buf) - oldpos
        seek(buf, oldpos)
        return true
    else
        return false
    end
end

function edit_werase(buf::IOBuffer)
    pos1 = position(buf)
    char_move_word_left(buf,isspace)
    pos0 = position(buf)
    pos0 < pos1 || return false
    memmove(buf, pos0+1, buf, pos1+1, buf.size-pos1)
    buf.size -= pos1 - pos0
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
    memmove(buf, pos0+1, buf, pos1+1, buf.size-pos1)
    buf.size -= pos1 - pos0
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
    seek(buf,pos0)
    memmove(buf, pos0+1, buf, pos1+1, buf.size-pos1)
    buf.size -= pos1 - pos0
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
    pos = position(buffer(s))
    s.kill_buffer = readline(buffer(s))
    rest = readall(buffer(s))
    truncate(buffer(s), pos)
    if !isempty(s.kill_buffer) && s.kill_buffer[end] == '\n'
        s.kill_buffer = s.kill_buffer[1:end-1]
        isempty(s.kill_buffer) || print(buffer(s), '\n')
    end
    print(buffer(s), rest)
    seek(buffer(s), pos)
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
    s.input_buffer = l
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
history_next_prefix(s, hist) = false
history_prev_prefix(s, hist) = false

function history_prev(s, hist)
    l, ok = history_prev(mode(s).hist)
    if ok
        replace_line(s, l)
        move_input_start(s)
        refresh_line(s)
    else
        beep(LineEdit.terminal(s))
    end
end
function history_next(s, hist)
    l, ok = history_next(mode(s).hist)
    if ok
        replace_line(s, l)
        move_input_end(s)
        refresh_line(s)
    else
        beep(LineEdit.terminal(s))
    end
end

refresh_line(s) = refresh_multi_line(s)
refresh_line(s,termbuf) = refresh_multi_line(termbuf,s)

default_completion_cb(::IOBuffer) = []
default_enter_cb(_) = true

write_prompt(terminal, s::PromptState) = write_prompt(terminal, s, s.p.prompt)
function write_prompt(terminal, s::PromptState,prompt)
    write(terminal, s.p.prompt_color)
    write(terminal, prompt)
    write(terminal, Base.text_colors[:normal])
    write(terminal, s.p.input_color)
end
write_prompt(terminal, s::ASCIIString) = write(terminal, s)

normalize_key(key::Char) = string(key)
normalize_key(key::Integer) = normalize_key(char(key))
function normalize_key(key::String)
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
            c, i == next(key, i)
            if c == 'C'
                c, i == next(key, i)
                @assert c == '-'
                c, i == next(key, i)
                write(buf, uppercase(c)-64)
            elseif c == 'M'
                c, i == next(key, i)
                @assert c == '-'
                c, i == next(key, i)
                write(buf, '\e')
                write(buf, c)
            end
        else
            write(buf, c)
        end
    end
    return takebuf_string(buf)
end

# Turn a Dict{Any,Any} into a Dict{Char,Any}
# For now we use \0 to represent unknown chars so that they are sorted before everything else
# If we ever actually want to match \0 in input, this will have to be reworked
function normalize_keymap(keymap::Dict)
    ret = Dict{Char,Any}()
    for key in keys(keymap)
        newkey = normalize_key(key)
        current = ret
        i = start(newkey)
        while !done(newkey, i)
            c, i = next(newkey, i)
            if haskey(current, c)
                if !isa(current[c], Dict)
                    println(ret)
                    error("Conflicting Definitions for keyseq " * escape_string(newkey) * " within one keymap")
                end
            elseif done(newkey, i)
                if isa(keymap[key], String)
                    current[c] = normalize_key(keymap[key])
                else
                    current[c] = keymap[key]
                end
                break
            else
                current[c] = Dict{Char,Any}()
            end
            current = current[c]
        end
    end
    ret
end

keymap_gen_body(keymaps, body::Expr, level) = body
keymap_gen_body(keymaps, body::Function, level) = keymap_gen_body(keymaps, :($(body)(s)))
keymap_gen_body(keymaps, body::Char, level) = keymap_gen_body(keymaps, keymaps[body])
keymap_gen_body(keymaps, body::Nothing, level) = nothing
function keymap_gen_body(keymaps, body::String, level)
    if length(body) == 1
        return keymap_gen_body(keymaps, body[1], level)
    end
    current = keymaps
    for c in body
        if haskey(current, c)
            if isa(current[c], Dict)
                current = current[c]
            else
                return keymap_gen_body(keymaps, current[c], level)
            end
        elseif haskey(current, '\0')
            return keymap_gen_body(keymaps, current['\0'], level)
        else
            error("No match for redirected key $body")
        end
    end
    error("No exact match for redirected key $body")
end

keymap_gen_body(a, b) = keymap_gen_body(a, b, 1)
function keymap_gen_body(dict, subdict::Dict, level)
    block = Expr(:block)
    bc = symbol("c" * string(level))
    push!(block.args, :($bc = read(LineEdit.terminal(s), Char)))

    if haskey(subdict, '\0')
        last_if = keymap_gen_body(dict, subdict['\0'], level+1)
    else
        last_if = nothing
    end

    for c in keys(subdict)
        c == '\0' && continue
        cblock = Expr(:if, :($bc == $c))
        push!(cblock.args, keymap_gen_body(dict, subdict[c], level+1))
        push!(cblock.args, last_if)
        last_if = cblock
    end

    push!(block.args, last_if)
    return block
end

export @keymap

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
    if level > 1
        for d in dict
            fixup_keymaps!(d[2], level-1, s, subkeymap)
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
        for s in keys(default_branch)
            s == '\0' && add_specialisations(dict, default_branch, level+1)
            fixup_keymaps!(dict, level, s, default_branch[s])
        end
    end
end

fix_conflicts!(x) = fix_conflicts!(x, 1)
fix_conflicts!(others, level) = nothing
function fix_conflicts!(dict::Dict, level)
    # needs to be done first for every branch
    if haskey(dict, '\0')
        add_specialisations(dict, dict, level)
    end
    for d in dict
        d[1] == '\0' && continue
        fix_conflicts!(d[2], level+1)
    end
end

keymap_prepare(keymaps::Expr) = keymap_prepare(eval(keymaps))
keymap_prepare(keymaps::Dict) = keymap_prepare([keymaps])
function keymap_prepare{D<:Dict}(keymaps::Array{D})
    push!(keymaps, {"*"=>:(error("Unrecognized input"))})
    keymaps = map(normalize_keymap, keymaps)
    map(fix_conflicts!, keymaps)
    keymaps
end

function keymap_unify(keymaps)
    length(keymaps) == 1 && return keymaps[1]
    ret = Dict{Char,Any}()
    for keymap in keymaps
        keymap_merge!(ret, keymap)
    end
    fix_conflicts!(ret)
    return ret
end

macro keymap(keymaps)
    dict = keymap_unify(keymap_prepare(keymaps))
    body = keymap_gen_body(dict, dict)
    esc(quote
        (s, data) -> begin
            $body
            return :ok
        end
    end)
end

const escape_defaults = {
    # Ignore other escape sequences by default
    "\e*" => nothing,
    "\e[*" => nothing,
    # Also ignore extended escape sequences
    # TODO: Support tanges of characters
    "\e[1**" => nothing,
    "\e[2**" => nothing,
    "\e[3**" => nothing,
    "\e[4**" => nothing,
    "\e[5**" => nothing,
    "\e[6**" => nothing,
    "\e[1~" => "\e[H",
    "\e[4~" => "\e[F",
    "\e[7~" => "\e[H",
    "\e[8~" => "\e[F",
    "\eOH"  => "\e[H",
    "\eOF"  => "\e[F",
}

function write_response_buffer(s::PromptState, data)
    offset = s.input_buffer.ptr
    ptr = data.response_buffer.ptr
    seek(data.response_buffer, 0)
    write(s.input_buffer, readall(data.response_buffer))
    s.input_buffer.ptr = offset + ptr - 2
    data.response_buffer.ptr = ptr
    refresh_line(s)
end

type SearchState
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
    history_search(data.histprompt.hp, data.query_buffer, data.response_buffer, data.backward, false) || beep(LineEdit.terminal(s))
    refresh_line(s)
end

function history_next_result(s::MIState, data::SearchState)
    history_search(data.histprompt.hp, data.query_buffer, data.response_buffer, data.backward, true) || beep(LineEdit.terminal(s))
    refresh_line(data)
end

function history_set_backward(s::SearchState, backward)
    s.backward = backward
end

refresh_multi_line(termbuf::TerminalBuffer, term, s::Union(SearchState,PromptState)) = (@assert term == terminal(s); refresh_multi_line(termbuf,s))
function refresh_multi_line(termbuf::TerminalBuffer, s::SearchState)
    buf = IOBuffer()
    write(buf, pointer(s.query_buffer.data), s.query_buffer.ptr-1)
    write(buf, "': ")
    offset = buf.ptr
    ptr = s.response_buffer.ptr
    seek(s.response_buffer, 0)
    write(buf, readall(s.response_buffer))
    buf.ptr = offset + ptr - 1
    s.response_buffer.ptr = ptr
    s.ias = refresh_multi_line(termbuf, s.terminal, buf, s.ias, s.backward ? "(reverse-i-search)`" : "(i-search)`")
end

function refresh_multi_line(s::Union(SearchState,PromptState))
    refresh_multi_line(terminal(s),s)
end

function refresh_multi_line(terminal::TTYTerminal, args...; kwargs...)
    outbuf = IOBuffer()
    termbuf = TerminalBuffer(outbuf)
    ret = refresh_multi_line(termbuf, terminal, args...;kwargs...)
    # Output the entire refresh at once
    write(terminal,takebuf_array(outbuf))
    flush(terminal)
    return ret
end

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

type HistoryPrompt <: TextInterface
    hp::HistoryProvider
    keymap_func::Function
    HistoryPrompt(hp) = new(hp)
end

init_state(terminal, p::HistoryPrompt) = SearchState(terminal, p, true, IOBuffer(), IOBuffer())

state(s::MIState, p) = s.mode_state[p]
state(s::PromptState, p) = (@assert s.p == p; s)
mode(s::MIState) = s.current_mode
mode(s::PromptState) = s.p
mode(s::SearchState) = @assert false

function accept_result(s, p)
    parent = state(s, p).parent
    replace_line(state(s, parent), state(s, p).response_buffer)
    transition(s, parent)
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
    buf = buffer(s)
    p.hp.last_mode = mode(s)
    p.hp.last_buffer = copy(buf)

    ss = state(s, p)
    ss.parent = mode(s)
    ss.backward = backward
    truncate(ss.query_buffer, 0)
    copybuf!(ss.response_buffer, buf)
    transition(s, p)
end

function setup_search_keymap(hp)
    p = HistoryPrompt(hp)
    pkeymap = {
        "^R"      => :(LineEdit.history_set_backward(data, true); LineEdit.history_next_result(s, data)),
        "^S"      => :(LineEdit.history_set_backward(data, false); LineEdit.history_next_result(s, data)),
        '\r'      => s->accept_result(s, p),
        '\n'      => '\r',
        '\t'      => nothing, #TODO: Maybe allow tab completion in R-Search?

        # Backspace/^H
        '\b'      => :(LineEdit.edit_backspace(data.query_buffer) ?
                        LineEdit.update_display_buffer(s, data) : beep(LineEdit.terminal(s))),
        127       => '\b',
        # Meta Backspace
        "\e\b"    => :(LineEdit.edit_delete_prev_word(data.query_buffer) ?
                        LineEdit.update_display_buffer(s, data) : beep(LineEdit.terminal(s))),
        "\e\x7f"  => "\e\b",
        # ^C and ^D
        "^C"      => :(LineEdit.edit_clear(data.query_buffer);
                       LineEdit.edit_clear(data.response_buffer);
                       LineEdit.update_display_buffer(s, data);
                       LineEdit.reset_state(data.histprompt.hp);
                       LineEdit.transition(s, data.parent)),
        "^D"      => "^C",
        # ^K
        11        => s->transition(s, state(s, p).parent),
        # ^Y
        25        => :(LineEdit.edit_yank(s); LineEdit.update_display_buffer(s, data)),
        # ^U
        21        => :(LineEdit.edit_clear(data.query_buffer);
                       LineEdit.edit_clear(data.response_buffer);
                       LineEdit.update_display_buffer(s, data)),
        # Right Arrow
        "\e[C"    => s->(accept_result(s, p); edit_move_right(s)),
        # Left Arrow
        "\e[D"    => s->(accept_result(s, p); edit_move_left(s)),
        # Up Arrow
        "\e[A"    => s->(accept_result(s, p); edit_move_up(s)),
        # Down Arrow
        "\e[B"    => s->(accept_result(s, p); edit_move_down(s)),
        # ^B
        2         => s->(accept_result(s, p); edit_move_left(s)),
        # ^F
        6         => s->(accept_result(s, p); edit_move_right(s)),
        # Meta B
        "\eb"     => s->(accept_result(s, p); edit_move_word_left(s)),
        # Meta F
        "\ef"     => s->(accept_result(s, p); edit_move_word_right(s)),
        # Ctrl-Left Arrow
        "\e[1;5D" => "\eb",
        # Ctrl-Right Arrow
        "\e[1;5C" => "\ef",
        # ^A
        1         => s->(accept_result(s, p); move_line_start(s); refresh_line(s)),
        # ^E
        5         => s->(accept_result(s, p); move_line_end(s); refresh_line(s)),
        "^Z"      => :(return :suspend),
        # Try to catch all Home/End keys
        "\e[H"    => s->(accept_result(s, p); move_input_start(s); refresh_line(s)),
        "\e[F"    => s->(accept_result(s, p); move_input_end(s); refresh_line(s)),
        "*"       => :(LineEdit.edit_insert(data.query_buffer, c1); LineEdit.update_display_buffer(s, data))
    }
    p.keymap_func = @eval @LineEdit.keymap $([pkeymap, escape_defaults])
    keymap = {
        "^R"    => s->(enter_search(s, p, true)),
        "^S"    => s->(enter_search(s, p, false)),
    }
    (p, keymap)
end

keymap(state, p::HistoryPrompt) = p.keymap_func
keymap_data(state, ::HistoryPrompt) = state

Base.isempty(s::PromptState) = s.input_buffer.size == 0

on_enter(s::PromptState) = s.p.on_enter(s)

move_input_start(s) = (seek(buffer(s), 0))
move_input_end(s) = (seekend(buffer(s)))
function move_line_start(s)
    buf = buffer(s)
    curpos = position(buf)
    curpos == 0 && return
    if buf.data[curpos] == '\n'
        move_input_start(s)
    else
        seek(buf, rsearch(buf.data, '\n', curpos-1))
    end
end
function move_line_end(s)
    buf = buffer(s)
    curpos = position(buf)
    eof(buf) && return
    c = read(buf, Char)
    if c == '\n'
        move_input_end(s)
        return
    end
    seek(buf, curpos)
    pos = search(buffer(s).data, '\n', curpos+1)
    if pos == 0
        move_input_end(s)
        return
    end
    seek(buf, pos-1)
end

function commit_line(s)
    LineEdit.move_input_end(s)
    LineEdit.refresh_line(s)
    println(LineEdit.terminal(s))
    LineEdit.add_history(s)
    LineEdit.state(s, LineEdit.mode(s)).ias =
        LineEdit.InputAreaState(0, 0)
end

const default_keymap =
{
    # Tab
    '\t' => s->begin
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
        LineEdit.complete_line(s)
        LineEdit.refresh_line(s)
    end,
    # Enter
    '\r' => quote
        if LineEdit.on_enter(s)
            LineEdit.commit_line(s)
            return :done
        else
            LineEdit.edit_insert(s, '\n')
        end
    end,
    '\n' => '\r',
    # Backspace/^H
    '\b' => edit_backspace,
    127 => '\b',
    # Meta Backspace
    "\e\b" => edit_delete_prev_word,
    "\e\x7f" => "\e\b",
    # ^D
    4 => quote
        if LineEdit.buffer(s).size > 0
            LineEdit.edit_delete(s)
        else
            println(LineEdit.terminal(s))
            return :abort
        end
    end,
    # ^B
    2 => edit_move_left,
    # ^F
    6 => edit_move_right,
    # Meta B
    "\eb" => edit_move_word_left,
    # Meta F
    "\ef" => edit_move_word_right,
    # Ctrl-Left Arrow
    "\e[1;5D" => "\eb",
    # Ctrl-Right Arrow
    "\e[1;5C" => "\ef",
    # Meta Enter
    "\e\r" => :(LineEdit.edit_insert(s, '\n')),
    "\e\n" => "\e\r",
    # Simply insert it into the buffer by default
    "*" => :(LineEdit.edit_insert(s, c1)),
    # ^U
    21 => edit_clear,
    # ^K
    11 => edit_kill_line,
    # ^Y
    25 => edit_yank,
    # ^A
    1 => :( LineEdit.move_line_start(s); LineEdit.refresh_line(s) ),
    # ^E
    5 => :( LineEdit.move_line_end(s); LineEdit.refresh_line(s) ),
    # Try to catch all Home/End keys
    "\e[H"  => :(LineEdit.move_input_start(s); LineEdit.refresh_line(s)),
    "\e[F"  => :(LineEdit.move_input_end(s); LineEdit.refresh_line(s)),
    # ^L
    12 => :(Terminals.clear(LineEdit.terminal(s)); LineEdit.refresh_line(s)),
    # ^W
    23 => edit_werase,
    # Meta D
    "\ed" => edit_delete_next_word,
    # ^C
    "^C" => s->begin
        move_input_end(s)
        LineEdit.refresh_line(s)
        print(LineEdit.terminal(s), "^C\n\n")
        transition(s, :reset)
        LineEdit.refresh_line(s)
    end,
    "^Z" => :(return :suspend),
    # Right Arrow
    "\e[C" => edit_move_right,
    # Left Arrow
    "\e[D" => edit_move_left,
    # Up Arrow
    "\e[A" => edit_move_up,
    # Down Arrow
    "\e[B" => edit_move_down,
    # Delete
    "\e[3~" => edit_delete,
    # Bracketed Paste Mode
    "\e[200~" => s->begin
        ps = state(s, mode(s))
        input = readuntil(ps.terminal, "\e[201~")[1:(end-6)]
        input = replace(input, '\r', '\n')
        if position(buffer(s)) == 0
            indent = Base.indentation(input)[1]
            input = Base.unindent(input[(indent+1):end], indent)
        end
        edit_insert(s, input)
    end,
    "^T"      => edit_transpose,
    # Unused and unprintable control character combinations
    "^G"      => nothing,
    "^O"      => nothing,
    "^Q"      => nothing,
    "^V"      => nothing,
    "^X"      => nothing,
}

function history_keymap(hist)
    return {
        # ^P
        16 => :(LineEdit.history_prev(s, $hist)),
        # ^N
        14 => :(LineEdit.history_next(s, $hist)),
        # Up Arrow
        "\e[A" => :(LineEdit.edit_move_up(s) || LineEdit.history_prev(s, $hist)),
        # Down Arrow
        "\e[B" => :(LineEdit.edit_move_down(s) || LineEdit.history_next(s, $hist)),
        # Page Up
        "\e[5~" => :(LineEdit.history_prev_prefix(s, $hist)),
        # Page Down
        "\e[6~" => :(LineEdit.history_next_prefix(s, $hist))
    }
end

function deactivate(p::Union(Prompt,HistoryPrompt), s::Union(SearchState,PromptState), termbuf)
    clear_input_area(termbuf,s)
    s
end

function activate(p::Union(Prompt,HistoryPrompt), s::Union(SearchState,PromptState), termbuf)
    s.ias = InputAreaState(0, 0)
    refresh_line(s,termbuf)
end

function activate(p::Union(Prompt,HistoryPrompt), s::MIState, termbuf)
    @assert p == s.current_mode
    activate(p, s.mode_state[s.current_mode], termbuf)
end
activate(m::ModalInterface, s::MIState, termbuf) = activate(s.current_mode, s, termbuf)

function transition(s::MIState, mode)
    if mode == :abort
        s.aborted = true
        return
    end
    if mode == :reset
        reset_state(s)
        return
    end
    termbuf = TerminalBuffer(IOBuffer())
    s.mode_state[s.current_mode] = deactivate(s.current_mode, s.mode_state[s.current_mode], termbuf)
    s.current_mode = mode
    activate(mode, s.mode_state[mode], termbuf)
    write(terminal(s),takebuf_array(termbuf.out_stream))
end

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

const default_keymap_func = @LineEdit.keymap [LineEdit.default_keymap, LineEdit.escape_defaults]

function Prompt(prompt;
    first_prompt = prompt,
    prompt_color = "",
    keymap_func = default_keymap_func,
    keymap_func_data = nothing,
    input_color = "",
    complete = EmptyCompletionProvider(),
    on_enter = default_enter_cb,
    on_done = ()->nothing,
    hist = EmptyHistoryProvider())

    Prompt(prompt, first_prompt, prompt_color, keymap_func, keymap_func_data,
           input_color, complete, on_enter, on_done, hist)
end

run_interface(::Prompt) = nothing

init_state(terminal, prompt::Prompt) = PromptState(terminal, prompt, IOBuffer(), InputAreaState(1, 1), length(prompt.prompt))

function init_state(terminal, m::ModalInterface)
    s = MIState(m, m.modes[1], false, Dict{Any,Any}(), "")
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

keymap(s::PromptState, prompt::Prompt) = prompt.keymap_func
keymap_data(s::PromptState, prompt::Prompt) = prompt.keymap_func_data
keymap(ms::MIState, m::ModalInterface) = keymap(ms.mode_state[ms.current_mode], ms.current_mode)
keymap_data(ms::MIState, m::ModalInterface) = keymap_data(ms.mode_state[ms.current_mode], ms.current_mode)

function prompt!(terminal, prompt, s = init_state(terminal, prompt))
    raw!(terminal, true)
    enable_bracketed_paste(terminal)
    try
        start_reading(terminal)
        activate(prompt, s, terminal)
        while true
            state = keymap(s, prompt)(s, keymap_data(s, prompt))
            if state == :abort
                stop_reading(terminal)
                return buffer(s), false, false
            elseif state == :done
                stop_reading(terminal)
                return buffer(s), true, false
            elseif state == :suspend
                @unix_only begin
                    stop_reading(terminal)
                    return buffer(s), true, true
                end
            else
                @assert state == :ok
            end
        end
    finally
        raw!(terminal, false) && disable_bracketed_paste(terminal)
    end
end

end # module
