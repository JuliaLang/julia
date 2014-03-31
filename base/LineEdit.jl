module LineEdit

using Base.Terminals

import Base.Terminals: raw!, width, height, cmove, getX,
                       getY, clear_line, beep

import Base: ensureroom, peek

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

input_string(s::PromptState) = bytestring(pointer(s.input_buffer.data),s.input_buffer.size)

abstract HistoryProvider
abstract CompletionProvider

type EmptyCompletionProvider <: CompletionProvider
end

type EmptyHistoryProvider <: HistoryProvider
end

reset_state(::EmptyHistoryProvider) = nothing

complete_line(c::EmptyCompletionProvider,s) = []

terminal(s::IO) = s
terminal(s::PromptState) = s.terminal

for f in [:terminal,:edit_insert,:on_enter,:add_history,:buffer,:edit_backspace,:(Base.isempty),
        :replace_line,:refreshMultiLine,:input_string,:complete_line,:edit_move_left,:edit_move_right,
        :edit_move_word_left,:edit_move_word_right,:update_display_buffer]
    @eval ($f)(s::MIState,args...) = $(f)(s.mode_state[s.current_mode],args...)
end

function common_prefix(completions)
    ret = ""
    i = nexti = 1
    cc,nexti = next(completions[1],1)
    while true
        for c in completions
            if i > length(c) || c[i] != cc
                return ret
            end
        end
        ret = ret*string(cc)
        if i >= length(completions[1])
            return ret
        end
        i = nexti
        cc,nexti = next(completions[1],i)
    end
end

# Show available completions
function show_completions(s::PromptState, completions)
    colmax = maximum(map(length,completions))
    num_cols = max(div(width(LineEdit.terminal(s)),colmax+2),1)
    entries_per_col, r = divrem(length(completions),num_cols)
    entries_per_col += r != 0
    println(LineEdit.terminal(s))
    for row = 1:entries_per_col
        for col = 0:num_cols
            idx = row + col*entries_per_col
            if idx <= length(completions)
                cmove_col(LineEdit.terminal(s),(colmax+2)*col)
                print(LineEdit.terminal(s),completions[idx])
            end
        end
        println(LineEdit.terminal(s))
    end
end

function complete_line(s::PromptState)
    (completions,partial,should_complete) = complete_line(s.p.complete,s)
    if length(completions) == 0
        beep(LineEdit.terminal(s))
    elseif !should_complete
        # should_complete is false for cases where we only want to show
        # a list of possible completions but not complete, e.g. foo(\t
        show_completions(s, completions)
    elseif length(completions) == 1
        # Replace word by completion
        prev_pos = position(s.input_buffer)
        seek(s.input_buffer,prev_pos-sizeof(partial))
        edit_replace(s,position(s.input_buffer),prev_pos,completions[1])
    else
        p = common_prefix(completions)
        if length(p) > 0 && p != partial
            # All possible completions share the same prefix, so we might as
            # well complete that
            prev_pos = position(s.input_buffer)
            seek(s.input_buffer,prev_pos-sizeof(partial))
            edit_replace(s,position(s.input_buffer),prev_pos,p)
        else
            show_completions(s, completions)
        end
    end
end

clear_input_area(s) = (clear_input_area(s.terminal,s.ias); s.ias = InputAreaState(0,0))
function clear_input_area(terminal,state::InputAreaState)
    #println(s.curs_row)
    #println(s.num_rows)

    # Go to the last line
    if state.curs_row < state.num_rows
        cmove_down(terminal,state.num_rows-state.curs_row)
    end

    # Clear lines one by one going up
    for j=0:(state.num_rows - 2)
        clear_line(terminal)
        cmove_up(terminal)
    end

    # Clear top line
    clear_line(terminal)
end

prompt_string(s::PromptState) = s.p.prompt
prompt_string(s::String) = s

refreshMultiLine(s::PromptState) = s.ias = refreshMultiLine(s.terminal,buffer(s),s.ias,s,indent=s.indent)

function refreshMultiLine(terminal,buf,state::InputAreaState,prompt = "";indent = 0)
    cols = width(terminal)

    clear_input_area(terminal,state)

    curs_row = -1 #relative to prompt
    curs_col = -1 #absolute
    curs_pos = -1 # 1 - based column position of the cursor
    cur_row = 0
    buf_pos = position(buf)
    line_pos = buf_pos
    # Write out the prompt string
    write_prompt(terminal,prompt)
    prompt = prompt_string(prompt)

    seek(buf,0)

    llength = 0

    l=""

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
                curs_row = div(plength+num_chars-1,cols)+1
                curs_pos = (plength+num_chars-1)%cols+1
            end
            cur_row += div(plength+llength-1,cols)
            line_pos -= slength
            write(terminal,l)
        else
            # We expect to be line after the last valid output line (due to
            # the '\n' at the end of the previous line)
            if curs_row == -1
                if line_pos < slength
                    num_chars = length(l[1:line_pos])
                    curs_row = cur_row+div(indent+num_chars-1,cols)
                    curs_pos = (indent+num_chars-1)%cols+1
                end
                line_pos -= slength #'\n' gets an extra pos
                cur_row += div(llength+indent-1,cols)
                cmove_col(terminal,indent+1)
                write(terminal,l)
                # There's an issue if the last character we wrote was at the very right end of the screen. In that case we need to
                # emit a new line and move the cursor there.
                if curs_pos == cols
                    write(terminal,"\n")
                    cmove_col(terminal,1)
                    curs_row+=1
                    curs_pos=0
                    cur_row+=1
                end
            else
                cur_row += div(llength+indent-1,cols)
                cmove_col(terminal,indent+1)
                write(terminal,l)
            end

        end


    end

    seek(buf,buf_pos)

    # If we are at the end of the buffer, we need to put the cursor one past the
    # last character we have written

    if curs_row == -1
        curs_pos = ((cur_row == 1 ? plength : indent)+llength-1)%cols+1
        curs_row = cur_row
    end

    # Same issue as above. TODO: We should figure out
    # how to refactor this to avoid duplcating functionality.
    if curs_pos == cols
        write(terminal,"\n")
        cmove_col(terminal,1)
        curs_row+=1
        curs_pos=0
        cur_row+=1
    end


    # Let's move the cursor to the right position
    # The line first
    n = cur_row-curs_row
    if n>0
        cmove_up(terminal,n)
    end

    #columns are 1 based
    cmove_col(terminal,curs_pos+1)

    flush(terminal)

    # Updated cur_row,curs_row
    return InputAreaState(cur_row,curs_row)
end


# Edit functionality

char_move_left(s::PromptState) = char_move_left(s.input_buffer)
function char_move_left(buf::IOBuffer)
    while position(buf)>0
        seek(buf,position(buf)-1)
        c = peek(buf)
        if ((c&0x80) == 0) || ((c&0xc0) == 0xc0)
            break
        end
    end
end

function edit_move_left(s::PromptState)
    if position(s.input_buffer)>0
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
    while position(buf) != buf.size
        seek(buf,position(buf)+1)
        if position(buf)==buf.size
            break
        end
        c = peek(buf)
        if ((c&0x80) == 0) || ((c&0xc0) == 0xc0)
            break
        end
    end
end

function char_move_word_right(buf::IOBuffer)
    while !eof(buf) && isspace(read(buf,Char))
    end
    while !eof(buf)
        c = peek(buf)
        if isspace(char(c))
            break
        end
        read(buf,Char)
    end
end

function char_move_word_left(buf::IOBuffer)
    while position(buf) > 0
        char_move_left(buf)
        c = peek(buf)
        if !isspace(char(c))
            break
        end
    end
    while position(buf) > 0
        char_move_left(buf)
        c = peek(buf)
        if isspace(char(c))
            read(buf,Uint8)
            break
        end
    end
end

char_move_word_right(s) = char_move_word_right(buffer(s))
char_move_word_left(s) = char_move_word_left(buffer(s))

function edit_move_right(s)
    if position(s.input_buffer)!=s.input_buffer.size
        #move to the next UTF8 character to the right
        char_move_right(s)
        refresh_line(s)
    end
end

function edit_move_word_right(s)
    if position(s.input_buffer)!=s.input_buffer.size
        char_move_word_right(s)
        refresh_line(s)
    end
end

## Move line up/down
# Querying the terminal is expensive, memory access is cheap
# so to find the current column, we find the offset for the start
# of the line.

function edit_move_up(buf::IOBuffer)
    npos = rsearch(buf.data,'\n',position(buf))
    if npos == 0 #we're in the first line
        return false
    end
    # We're interested in character count, not byte count
    offset = length(bytestring(buf.data[(npos+1):(position(buf))]))
    npos2 = rsearch(buf.data,'\n',npos-1)
    seek(buf,npos2)
    for _ = 1:offset
        pos = position(buf)
        if read(buf,Char) == '\n'
            seek(buf,pos)
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
    npos = rsearch(buf.data[1:buf.size],'\n',position(buf))
    # We're interested in character count, not byte count
    offset = length(bytestring(buf.data[(npos+1):(position(buf))]))
    npos2 = search(buf.data[1:buf.size],'\n',position(buf)+1)
    if npos2 == 0 #we're in the last line
        return false
    end
    seek(buf,npos2)
    for _ = 1:offset
        pos = position(buf)
        if eof(buf) || read(buf,Char) == '\n'
            seek(buf,pos)
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

function edit_replace(s,from,to,str)
    room = length(str.data)-(to-from)
    ensureroom(s.input_buffer, s.input_buffer.size + room)
    memmove(s.input_buffer, to+room+1, s.input_buffer, to+1, s.input_buffer.size-to)
    s.input_buffer.size += room
    seek(s.input_buffer,from)
    write(s.input_buffer,str)
end

function edit_insert(s::PromptState,c)
    str = string(c)
    edit_insert(s.input_buffer,str)
    if !('\n' in str) && eof(s.input_buffer) &&
        ((position(s.input_buffer) + length(s.p.prompt) + sizeof(str) - 1) < width(LineEdit.terminal(s)))
        #Avoid full update
        write(LineEdit.terminal(s),str)
    else
        refresh_line(s)
    end
end

# TODO: Don't use memmove
function edit_insert(buf::IOBuffer,c)
    if eof(buf)
        write(buf,c)
    else
        s = string(c)
        ensureroom(buf,buf.size-position(buf)+sizeof(s))
        oldpos = position(buf)
        memmove(buf, position(buf)+1+sizeof(s), buf, position(buf)+1, buf.size-position(buf))
        buf.size += sizeof(s)
        write(buf,c)
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
    if position(buf) > 0 && buf.size>0
        oldpos = position(buf)
        char_move_left(buf)
        memmove(buf, position(buf)+1, buf, oldpos+1, buf.size-oldpos)
        buf.size -= oldpos-position(buf)
        return true
    else
        return false
    end
end

function edit_delete(s)
    buf = buffer(s)
    if buf.size>0 && position(buf) < buf.size
        oldpos = position(buf)
        char_move_right(s)
        memmove(buf, oldpos+1, buf, position(buf)+1, buf.size-position(buf))
        buf.size -= position(buf)-oldpos
        seek(buf,oldpos)
        refresh_line(s)
    else
        beep(LineEdit.terminal(s))
    end
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
    edit_insert(buffer(s),s.kill_buffer)
    refresh_line(s)
end

function edit_kill(s::MIState)
    pos = position(buffer(s))
    s.kill_buffer = readall(buffer(s))
    truncate(buffer(s),pos);
    refresh_line(s)
end

function replace_line(s::PromptState,l::IOBuffer)
    s.input_buffer = l
end

function replace_line(s::PromptState,l)
    s.input_buffer.ptr = 1
    s.input_buffer.size = 0
    write(s.input_buffer,l)
end

history_prev(::EmptyHistoryProvider) = ("",false)
history_next(::EmptyHistoryProvider) = ("",false)
history_search(::EmptyHistoryProvider,args...) = false
add_history(::EmptyHistoryProvider,s) = nothing
add_history(s::PromptState) = add_history(mode(s).hist,s)

function history_prev(s,hist)
    (l,ok) = history_prev(mode(s).hist)
    if ok
        replace_line(s,l)
        move_input_start(s)
    else
        beep(LineEdit.terminal(s))
    end
end
function history_next(s,hist)
    (l,ok) = history_next(mode(s).hist)
    if ok
        replace_line(s,l)
        move_input_end(s)
    else
        beep(LineEdit.terminal(s))
    end
end

refresh_line(s) = refreshMultiLine(s)

default_completion_cb(::IOBuffer) = []
default_enter_cb(_) = true

write_prompt(terminal,s::PromptState) = write_prompt(terminal,s,s.p.prompt)
function write_prompt(terminal,s::PromptState,prompt)
    @assert terminal == LineEdit.terminal(s)
    write(terminal,s.p.prompt_color)
    write(terminal,prompt)
    write(terminal,Base.text_colors[:normal])
    write(terminal,s.p.input_color)
end
write_prompt(terminal,s::ASCIIString) = write(terminal,s)

function normalize_key(key)
    if isa(key,Char)
        return string(key)
    elseif isa(key,Integer)
        return string(char(key))
    elseif isa(key,String)
        if in('\0',key)
            error("Matching \\0 not currently supported.")
        end
        buf = IOBuffer()
        i = start(key)
        while !done(key,i)
            (c,i) = next(key,i)
            if c == '*'
                write(buf,'\0')
            elseif c == '^'
                (c,i) = next(key,i)
                write(buf,uppercase(c)-64)
            elseif c == '\\'
                (c,i) == next(key,i)
                if c == 'C'
                    (c,i) == next(key,i)
                    @assert c == '-'
                    (c,i) == next(key,i)
                    write(buf,uppercase(c)-64)
                elseif c == 'M'
                    (c,i) == next(key,i)
                    @assert c == '-'
                    (c,i) == next(key,i)
                    write(buf,'\e')
                    write(buf,c)
                end
            else
                write(buf,c)
            end
        end
        return takebuf_string(buf)
    end
end

# Turn an Dict{Any,Any} into a Dict{'Char',Any}
# For now we use \0 to represent unknown chars so that they are sorted before everything else
# If we ever actually want to mach \0 in input, this will have to be
# reworked
function normalize_keymap(keymap)
    ret = Dict{Char,Any}()
    for key in keys(keymap)
        newkey = normalize_key(key)
        current = ret
        i = start(newkey)
        while !done(newkey,i)
            (c,i) = next(newkey,i)
            if haskey(current,c)
                if !isa(current[c],Dict)
                    println(ret)
                    error("Conflicting Definitions for keyseq "*escape_string(newkey)*" within one keymap")
                end
            elseif done(newkey,i)
                if isa(keymap[key],String)
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

keymap_gen_body(keymaps,body::Expr,level) = body
keymap_gen_body(keymaps,body::Function,level) = keymap_gen_body(keymaps,:($(body)(s)))
keymap_gen_body(keymaps,body::Char,level) = keymap_gen_body(keymaps,keymaps[body])
keymap_gen_body(keymaps,body::Nothing,level) = nothing
function keymap_gen_body(keymaps,body::String,level)
    if length(body) == 1
        return keymap_gen_body(keymaps,body[1],level)
    end
    current = keymaps
    for c in body
        if haskey(current,c)
            if isa(current[c],Dict)
                current = current[c]
            else
                return keymap_gen_body(keymaps,current[c],level)
            end
        elseif haskey(current,'\0')
            return keymap_gen_body(keymaps,current['\0'],level)
        else
            error("No match for redirected key $body")
        end
    end
    error("No exact match for redirected key $body")
end

keymap_gen_body(a,b) = keymap_gen_body(a,b,1)
function keymap_gen_body(dict,subdict::Dict,level)
    block = Expr(:block)
    bc = symbol("c"*string(level))
    push!(block.args,:($bc=read(LineEdit.terminal(s),Char)))

    if haskey(subdict,'\0')
        last_if = keymap_gen_body(dict,subdict['\0'],level+1)
    else
        last_if = nothing
    end

    for c in keys(subdict)
        if c == '\0'
            continue
        end
        cblock = Expr(:if,:($bc==$c))
        push!(cblock.args,keymap_gen_body(dict,subdict[c],level+1))
        if isa(cblock,Expr)
            push!(cblock.args,last_if)
        end
        last_if = cblock
    end

    push!(block.args,last_if)
    return block
end

export @keymap

# deep merge where target has higher precedence
function keymap_merge!(target::Dict,source::Dict)
    for k in keys(source)
        if !haskey(target,k)
            target[k] = source[k]
        elseif isa(target[k],Dict)
            keymap_merge!(target[k],source[k])
        else
            # Ignore, target has higher precedence
        end
    end
end

fixup_keymaps!(d,l,s,sk) = nothing
function fixup_keymaps!(dict::Dict, level, s, subkeymap)
    if level > 1
        for d in dict
            fixup_keymaps!(d[2],level-1,s,subkeymap)
        end
    else
        if haskey(dict,s)
            if isa(dict[s],Dict) && isa(subkeymap,Dict)
                keymap_merge!(dict[s],subkeymap)
            end
        else
            dict[s] = deepcopy(subkeymap)
        end
    end
end

function add_specialisations(dict,subdict,level)
    default_branch = subdict['\0']
    if isa(default_branch,Dict)
        for s in keys(default_branch)
            if s == '\0'
                add_specialisations(dict,default_branch,level+1)
            end
            fixup_keymaps!(dict,level,s,default_branch[s])
        end
    end
end

fix_conflicts!(x) = fix_conflicts!(x,1)
fix_conflicts!(others,level) = nothing
function fix_conflicts!(dict::Dict,level)
    # needs to be done first for every branch
    if haskey(dict,'\0')
        add_specialisations(dict,dict,level)
    end
    for d in dict
        if d[1] == '\0'
            continue
        end
        fix_conflicts!(d[2],level+1)
    end
end

function keymap_prepare(keymaps)
    if isa(keymaps,Dict)
        keymaps = [keymaps]
    end
    push!(keymaps,{"*"=>:(error("Unrecognized input"))})
    @assert isa(keymaps,Array) && eltype(keymaps) <: Dict
    keymaps = map(normalize_keymap,keymaps)
    map(fix_conflicts!,keymaps)
    keymaps
end

function keymap_unify(keymaps)
    if length(keymaps) == 1
        return keymaps[1]
    else
        ret = Dict{Char,Any}()
        for keymap in keymaps
            keymap_merge!(ret,keymap)
        end
        fix_conflicts!(ret)
        return ret
    end
end

macro keymap(func, keymaps)
    dict = keymap_unify(keymap_prepare(isa(keymaps,Expr)?eval(keymaps):keymaps))
    body = keymap_gen_body(dict,dict)
    esc(quote
        function $(func)(s,data)
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

function write_response_buffer(s::PromptState,data)
    offset = s.input_buffer.ptr
    ptr = data.respose_buffer.ptr
    seek(data.respose_buffer,0)
    write(s.input_buffer,readall(data.respose_buffer))
    s.input_buffer.ptr = offset+ptr-2
    data.respose_buffer.ptr = ptr
    refresh_line(s)
end

type SearchState
    terminal
    histprompt
    #rsearch (true) or ssearch (false)
    backward::Bool
    query_buffer::IOBuffer
    respose_buffer::IOBuffer
    ias::InputAreaState
    #The prompt whose input will be replaced by the matched history
    parent
    SearchState(a,b,c,d,e) = new(a,b,c,d,e,InputAreaState(0,0))
end

terminal(s::SearchState) = s.terminal

function update_display_buffer(s::SearchState,data)
    history_search(data.histprompt.hp,data.query_buffer,data.respose_buffer,data.backward,false) || beep(LineEdit.terminal(s))
    refresh_line(s)
end

function history_next_result(s::MIState,data::SearchState)
    #truncate(data.query_buffer,s.input_buffer.size - data.respose_buffer.size)
    history_search(data.histprompt.hp,data.query_buffer,data.respose_buffer,data.backward,true) || beep(LineEdit.terminal(s))
    refresh_line(data)
end

function history_set_backward(s::SearchState,backward)
    s.backward = backward
end

function refreshMultiLine(s::SearchState)
    buf = IOBuffer()
    write(buf,pointer(s.query_buffer.data),s.query_buffer.ptr-1)
    write(buf,"': ")
    offset = buf.ptr
    ptr = s.respose_buffer.ptr
    seek(s.respose_buffer,0)
    write(buf,readall(s.respose_buffer))
    buf.ptr = offset+ptr-1
    s.respose_buffer.ptr = ptr
    refreshMultiLine(s.terminal,buf,s.ias,s.backward ? "(reverse-i-search)`" : "(i-search)`")
end

function reset_state(s::SearchState)
    if s.query_buffer.size != 0
        s.query_buffer.size = 0
        s.query_buffer.ptr = 1
    end
    if s.respose_buffer.size != 0
        s.respose_buffer.size = 0
        s.query_buffer.ptr = 1
    end
    reset_state(s.histprompt.hp)
end

type HistoryPrompt <: TextInterface
    hp::HistoryProvider
    keymap_func::Function
    HistoryPrompt(hp) = new(hp)
end

init_state(terminal,p::HistoryPrompt) = SearchState(terminal,p,true,IOBuffer(),IOBuffer())

state(s::MIState,p) = s.mode_state[p]
state(s::PromptState,p) = (@assert s.p == p; s)
mode(s::MIState) = s.current_mode
mode(s::PromptState) = s.p
mode(s::SearchState) = @assert false

function accept_result(s,p)
    parent = state(s,p).parent
    replace_line(state(s,parent),state(s,p).respose_buffer)
    transition(s,parent)
end

function setup_search_keymap(hp)
    p = HistoryPrompt(hp)
    pkeymap = {
        "^R"    => :( LineEdit.history_set_backward(data,true); LineEdit.history_next_result(s,data) ),
        "^S"    => :( LineEdit.history_set_backward(data,false); LineEdit.history_next_result(s,data) ),
        "\r"    => s->accept_result(s,p),
        "\t"    => nothing, #TODO: Maybe allow tab completion in R-Search?

        # Backspace/^H
        '\b'    => :(LineEdit.edit_backspace(data.query_buffer)?LineEdit.update_display_buffer(s,data):beep(LineEdit.terminal(s))),
        127     => '\b',
        "^C"    => s->transition(s,state(s,p).parent),
        "^D"    => s->transition(s,state(s,p).parent),
        # ^K
        11      => s->transition(s,state(s,p).parent),
        # ^Y
        25      => :(LineEdit.edit_yank(s); LineEdit.update_display_buffer(s,data)),
        # ^A
        1       => s->(accept_result(s,p); move_line_start(s)),
        # ^E
        5       => s->(accept_result(s,p); move_line_end(s)),
        # Try to catch all Home/End keys
        "\e[H"  => s->(accept_result(s,p); move_input_start(s)),
        "\e[F"  => s->(accept_result(s,p); move_input_end(s)),
        "*"     => :(LineEdit.edit_insert(data.query_buffer,c1);LineEdit.update_display_buffer(s,data))
    }
    @eval @LineEdit.keymap keymap_func $([pkeymap, escape_defaults])
    p.keymap_func = keymap_func
    keymap = {
        "^R"    => s->( state(s,p).parent = mode(s); state(s,p).backward = true; transition(s,p) ),
        "^S"    => s->( state(s,p).parent = mode(s); state(s,p).backward = false; transition(s,p) ),
    }
    (p,keymap)
end

keymap(state,p::HistoryPrompt) = p.keymap_func
keymap_data(state,::HistoryPrompt) = state

Base.isempty(s::PromptState) = s.input_buffer.size == 0

on_enter(s::PromptState) = s.p.on_enter(s)

move_input_start(s) = (seek(buffer(s),0); refresh_line(s))
move_input_end(s) = (seekend(buffer(s)); refresh_line(s))
function move_line_start(s)
    buf = buffer(s)
    curpos = position(buf)
    if curpos == 0
        return
    end
    if buf.data[curpos] == '\n'
        move_input_start(s)
    else
        seek(buf,rsearch(buf.data,'\n',curpos-1))
    end
    refresh_line(s)
end
function move_line_end(s)
    buf = buffer(s)
    curpos = position(buf)
    if eof(buf)
        return
    end
    c = read(buf,Char)
    if c == '\n'
        move_input_end(s)
        return
    end
    seek(buf,curpos)
    pos = search(buffer(s).data,'\n',curpos+1)
    if pos == 0
        move_input_end(s)
        return
    end
    seek(buf,pos-1)
    refresh_line(s)
end

function commit_line(s)
    LineEdit.move_input_end(s)
    println(LineEdit.terminal(s))
    LineEdit.add_history(s)
    LineEdit.state(s,LineEdit.mode(s)).ias =
        LineEdit.InputAreaState(0,0)
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
                edit_insert(s," "^4)
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
            LineEdit.edit_insert(s,'\n')
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
    # Meta Enter
    "\e\r" => :(LineEdit.edit_insert(s,'\n')),
    # Simply insert it into the buffer by default
    "*" => :( LineEdit.edit_insert(s,c1) ),
    # ^U
    21 => :( truncate(LineEdit.buffer(s),0); LineEdit.refresh_line(s) ),
    # ^K
    11 => edit_kill,
    # ^Y
    25 => edit_yank,
    # ^A
    1 => move_line_start,
    # ^E
    5 => move_line_end,
    # Try to catch all Home/End keys
    "\e[H"  => move_input_start,
    "\e[F"  => move_input_end,
    # ^L
    12 => :( Terminals.clear(LineEdit.terminal(s)); LineEdit.refresh_line(s) ),
    # ^W
    23 => edit_delete_prev_word,
    # Meta D
    "\ed" => edit_delete_next_word,
    # ^C
    "^C" => s->begin
        move_input_end(s);
        LineEdit.refresh_line(s);
        print(LineEdit.terminal(s), "^C\n\n");
        transition(s,:reset);
        LineEdit.refresh_line(s)
    end,
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
        ps = state(s,mode(s))
        input = readuntil(ps.terminal,"\e[201~")[1:(end-6)]
        input = replace(input,'\r','\n')
        if position(buffer(s)) == 0
            indent = Base.indentation(input)[1]
            input = Base.unindent(input[(indent+1):end],indent)
        end
        edit_insert(s,input)
    end,
}

function history_keymap(hist)
    return {
        # ^P
        16 => :( LineEdit.history_prev(s,$hist) ),
        # ^N
        14 => :( LineEdit.history_next(s,$hist) ),
        # Up Arrow
        "\e[A" => :( LineEdit.edit_move_up(s) || LineEdit.history_prev(s,$hist) ),
        # Down Arrow
        "\e[B" => :( LineEdit.edit_move_down(s) || LineEdit.history_next(s,$hist) )
    }
end

function deactivate(p::Union(Prompt,HistoryPrompt),s::Union(SearchState,PromptState))
    clear_input_area(s)
    s
end

function activate(p::Union(Prompt,HistoryPrompt),s::Union(SearchState,PromptState))
    s.ias = InputAreaState(0,0)
    refresh_line(s)
end

function activate(p::Union(Prompt,HistoryPrompt),s::MIState)
    @assert p == s.current_mode
    activate(p,s.mode_state[s.current_mode])
end
activate(m::ModalInterface,s::MIState) = activate(s.current_mode,s)

function transition(s::MIState,mode)
    if mode == :abort
        s.aborted = true
        return
    end
    if mode == :reset
        reset_state(s)
        return
    end
    s.mode_state[s.current_mode] = deactivate(s.current_mode,s.mode_state[s.current_mode])
    s.current_mode = mode
    activate(mode,s.mode_state[mode])
end

function reset_state(s::PromptState)
    if s.input_buffer.size != 0
        s.input_buffer.size = 0
        s.input_buffer.ptr = 1
    end
    s.ias = InputAreaState(0,0)
end

function reset_state(s::MIState)
    for (mode,state) in s.mode_state
        reset_state(state)
    end
end

@LineEdit.keymap default_keymap_func [LineEdit.default_keymap,LineEdit.escape_defaults]

function Prompt(prompt;
    first_prompt = prompt,
    prompt_color="",
    keymap_func = default_keymap_func,
    keymap_func_data = nothing,
    input_color="",
    complete=EmptyCompletionProvider(),
    on_enter=default_enter_cb,on_done=()->nothing,hist=EmptyHistoryProvider())
    Prompt(prompt,first_prompt,prompt_color,keymap_func,keymap_func_data,input_color,complete,on_enter,on_done,hist)
end

function run_interface(::Prompt)

end

init_state(terminal,prompt::Prompt) = PromptState(terminal,prompt,IOBuffer(),InputAreaState(1,1),length(prompt.prompt))

function init_state(terminal,m::ModalInterface)
    s = MIState(m,m.modes[1],false,Dict{Any,Any}(),"")
    for mode in m.modes
        s.mode_state[mode] = init_state(terminal,mode)
    end
    s
end

function run_interface(terminal,m::ModalInterface)
    s = init_state(terminal,m)
    while !s.aborted
        p = s.current_mode
        buf,ok = prompt!(terminal,m,s)
        s.mode_state[s.current_mode].p.on_done(s,buf,ok)
    end
end

buffer(s::PromptState) = s.input_buffer
buffer(s::SearchState) = s.query_buffer

keymap(s::PromptState,prompt::Prompt) = prompt.keymap_func
keymap_data(s::PromptState,prompt::Prompt) = prompt.keymap_func_data
keymap(ms::MIState,m::ModalInterface) = keymap(ms.mode_state[ms.current_mode],ms.current_mode)
keymap_data(ms::MIState,m::ModalInterface) = keymap_data(ms.mode_state[ms.current_mode],ms.current_mode)

function prompt!(terminal,prompt,s=init_state(terminal,prompt))
    raw!(terminal,true)
    enable_bracketed_paste(terminal)
    try
        start_reading(terminal)
        activate(prompt,s)
        while true
            state = keymap(s,prompt)(s,keymap_data(s,prompt))
            if state == :abort
                stop_reading(terminal)
                return (buffer(s),false)
            elseif state == :done
                stop_reading(terminal)
                return (buffer(s),true)
            else
                @assert state == :ok
            end
        end
    finally
        raw!(terminal,false) && disable_bracketed_paste(terminal)
    end
end

end # module
