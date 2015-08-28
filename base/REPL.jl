# This file is a part of Julia. License is MIT: http://julialang.org/license

module REPL

using Base.Meta
using ..Terminals
using ..LineEdit
using ..REPLCompletions

export
    BasicREPL,
    LineEditREPL,
    StreamREPL

import Base:
    Display,
    display,
    writemime,
    AnyDict,
    ==

import ..LineEdit:
    CompletionProvider,
    HistoryProvider,
    add_history,
    complete_line,
    history_next,
    history_next_prefix,
    history_prev,
    history_prev_prefix,
    history_search,
    accept_result,
    terminal

abstract AbstractREPL

answer_color(::AbstractREPL) = ""

type REPLBackend
    repl_channel::Channel
    response_channel::Channel
    in_eval::Bool
    ans
    backend_task::Task
    REPLBackend(repl_channel, response_channel, in_eval, ans) =
        new(repl_channel, response_channel, in_eval, ans)
end

function eval_user_input(ast::ANY, backend::REPLBackend)
    iserr, lasterr, bt = false, (), nothing
    while true
        try
            if iserr
                put!(backend.response_channel, (lasterr, bt))
                iserr, lasterr = false, ()
            else
                ans = backend.ans
                # note: value wrapped in a non-syntax value to avoid evaluating
                # possibly-invalid syntax (issue #6763).
                backend.in_eval = true
                eval(Main, :(ans = $(Any[ans])[1]))
                backend.in_eval = false
                value = eval(Main, ast)
                backend.ans = value
                put!(backend.response_channel, (value, nothing))
            end
            break
        catch err
            if iserr
                println("SYSTEM ERROR: Failed to report error to REPL frontend")
                println(err)
            end
            iserr, lasterr = true, err
            bt = catch_backtrace()
        end
    end
end

function start_repl_backend(repl_channel::Channel, response_channel::Channel)
    backend = REPLBackend(repl_channel, response_channel, false, nothing)
    backend.backend_task = @schedule begin
        # include looks at this to determine the relative include path
        # nothing means cwd
        while true
            tls = task_local_storage()
            tls[:SOURCE_PATH] = nothing
            ast, show_value = take!(backend.repl_channel)
            if show_value == -1
                # exit flag
                break
            end
            eval_user_input(ast, backend)
        end
    end
    backend
end

function display_error(io::IO, er, bt)
    Base.with_output_color(:red, io) do io
        print(io, "ERROR: ")
        Base.showerror(io, er, bt)
    end
end

immutable REPLDisplay{R<:AbstractREPL} <: Display
    repl::R
end

==(a::REPLDisplay, b::REPLDisplay) = a.repl === b.repl

function display(d::REPLDisplay, ::MIME"text/plain", x)
    io = outstream(d.repl)
    Base.have_color && write(io, answer_color(d.repl))
    writemime(io, MIME("text/plain"), x)
    println(io)
end
display(d::REPLDisplay, x) = display(d, MIME("text/plain"), x)

function print_response(repl::AbstractREPL, val::ANY, bt, show_value::Bool, have_color::Bool)
    repl.waserror = bt !== nothing
    print_response(outstream(repl), val, bt, show_value, have_color, specialdisplay(repl))
end
function print_response(errio::IO, val::ANY, bt, show_value::Bool, have_color::Bool, specialdisplay=nothing)
    while true
        try
            if bt !== nothing
                display_error(errio, val, bt)
                println(errio)
                iserr, lasterr = false, ()
            else
                if val !== nothing && show_value
                    try
                        if specialdisplay === nothing
                            display(val)
                        else
                            display(specialdisplay,val)
                        end
                    catch err
                        println(errio, "Error showing value of type ", typeof(val), ":")
                        rethrow(err)
                    end
                end
            end
            break
        catch err
            if bt !== nothing
                println(errio, "SYSTEM: show(lasterr) caused an error")
                break
            end
            val = err
            bt = catch_backtrace()
        end
    end
end

# A reference to a backend
immutable REPLBackendRef
    repl_channel::Channel
    response_channel::Channel
end

function run_repl(repl::AbstractREPL)
    repl_channel = Channel(1)
    response_channel = Channel(1)
    backend = start_repl_backend(repl_channel, response_channel)
    run_frontend(repl, REPLBackendRef(repl_channel,response_channel))
    backend
end

## BasicREPL ##

type BasicREPL <: AbstractREPL
    terminal::TextTerminal
    waserror::Bool
    BasicREPL(t) = new(t,false)
end

outstream(r::BasicREPL) = r.terminal

function run_frontend(repl::BasicREPL, backend::REPLBackendRef)
    d = REPLDisplay(repl)
    dopushdisplay = !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    repl_channel, response_channel = backend.repl_channel, backend.response_channel
    hit_eof = false
    while true
        Base.reseteof(repl.terminal)
        write(repl.terminal, "julia> ")
        line = ""
        ast = nothing
        interrupted = false
        while true
            try
                line *= readline(repl.terminal)
            catch e
                if isa(e,InterruptException)
                    try # raise the debugger if present
                        ccall(:jl_raise_debugger, Int, ())
                    end
                    line = ""
                    interrupted = true
                    break
                elseif isa(e,EOFError)
                    hit_eof = true
                    break
                else
                    rethrow()
                end
            end
            ast = Base.parse_input_line(line)
            (isa(ast,Expr) && ast.head == :incomplete) || break
        end
        if !isempty(line)
            put!(repl_channel, (ast, 1))
            val, bt = take!(response_channel)
            if !ends_with_semicolon(line)
                print_response(repl, val, bt, true, false)
            end
        end
        write(repl.terminal, '\n')
        ((!interrupted && isempty(line)) || hit_eof) && break
    end
    # terminate backend
    put!(repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
end

## LineEditREPL ##

type LineEditREPL <: AbstractREPL
    t::TextTerminal
    hascolor::Bool
    prompt_color::AbstractString
    input_color::AbstractString
    answer_color::AbstractString
    shell_color::AbstractString
    help_color::AbstractString
    history_file::Bool
    in_shell::Bool
    in_help::Bool
    envcolors::Bool
    waserror::Bool
    specialdisplay
    interface
    backendref::REPLBackendRef
    LineEditREPL(t,hascolor,prompt_color,input_color,answer_color,shell_color,help_color,history_file,in_shell,in_help,envcolors) =
        new(t,true,prompt_color,input_color,answer_color,shell_color,help_color,history_file,in_shell,
            in_help,envcolors,false,nothing)
end
outstream(r::LineEditREPL) = r.t
specialdisplay(r::LineEditREPL) = r.specialdisplay
specialdisplay(r::AbstractREPL) = nothing
terminal(r::LineEditREPL) = r.t

LineEditREPL(t::TextTerminal, envcolors = false) =  LineEditREPL(t,
                                              true,
                                              julia_green,
                                              Base.input_color(),
                                              Base.answer_color(),
                                              Base.text_colors[:red],
                                              Base.text_colors[:yellow],
                                              false, false, false, envcolors)

type REPLCompletionProvider <: CompletionProvider
    r::LineEditREPL
end

type ShellCompletionProvider <: CompletionProvider
    r::LineEditREPL
end

immutable LatexCompletions <: CompletionProvider; end

bytestring_beforecursor(buf::IOBuffer) = bytestring(buf.data[1:buf.ptr-1])

function complete_line(c::REPLCompletionProvider, s)
    partial = bytestring_beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = completions(full, endof(partial))
    return ret, partial[range], should_complete
end

function complete_line(c::ShellCompletionProvider, s)
    # First parse everything up to the current position
    partial = bytestring_beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = shell_completions(full, endof(partial))
    return ret, partial[range], should_complete
end

function complete_line(c::LatexCompletions, s)
    partial = bytestring_beforecursor(LineEdit.buffer(s))
    full = LineEdit.input_string(s)
    ret, range, should_complete = bslash_completions(full, endof(partial))[2]
    return ret, partial[range], should_complete
end


type REPLHistoryProvider <: HistoryProvider
    history::Array{AbstractString,1}
    history_file
    cur_idx::Int
    last_idx::Int
    last_buffer::IOBuffer
    last_mode
    mode_mapping
    modes::Array{Symbol,1}
end
REPLHistoryProvider(mode_mapping) =
    REPLHistoryProvider(AbstractString[], nothing, 0, -1, IOBuffer(),
                        nothing, mode_mapping, UInt8[])

const invalid_history_message = """
Invalid history file (~/.julia_history) format:
If you have a history file left over from an older version of Julia,
try renaming or deleting it.
Invalid character: """

const munged_history_message = """
Invalid history file (~/.julia_history) format:
An editor may have converted tabs to spaces at line """

function hist_getline(file)
    while !eof(file)
        line = utf8(readline(file))
        isempty(line) && return line
        line[1] in "\r\n" || return line
    end
    return utf8("")
end

function hist_from_file(hp, file)
    hp.history_file = file
    seek(file, 0)
    countlines = 0
    while true
        mode = :julia
        line = hist_getline(file)
        isempty(line) && break
        countlines += 1
        line[1] != '#' &&
            error(invalid_history_message, repr(line[1]), " at line ", countlines)
        while !isempty(line)
            m = match(r"^#\s*(\w+)\s*:\s*(.*?)\s*$", line)
            m === nothing && break
            if m.captures[1] == "mode"
                mode = symbol(m.captures[2])
            end
            line = hist_getline(file)
            countlines += 1
        end
        isempty(line) && break
        # Make sure starts with tab
        line[1] == ' '  &&
            error(munged_history_message, countlines)
        line[1] != '\t' &&
            error(invalid_history_message, repr(line[1]), " at line ", countlines)
        lines = UTF8String[]
        while !isempty(line)
            push!(lines, chomp(line[2:end]))
            eof(file) && break
            ch = Base.peek(file)
            ch == ' '  && error(munged_history_message, countlines)
            ch != '\t' && break
            line = hist_getline(file)
            countlines += 1
        end
        push!(hp.modes, mode)
        push!(hp.history, join(lines, '\n'))
    end
    seekend(file)
    hp
end

function mode_idx(hist::REPLHistoryProvider, mode)
    c = :julia
    for (k,v) in hist.mode_mapping
        v == mode && (c = k)
    end
    return c
end

function add_history(hist::REPLHistoryProvider, s)
    str = rstrip(bytestring(s.input_buffer))
    isempty(strip(str)) && return
    mode = mode_idx(hist, LineEdit.mode(s))
    length(hist.history) > 0 &&
        mode == hist.modes[end] && str == hist.history[end] && return
    push!(hist.modes, mode)
    push!(hist.history, str)
    hist.history_file === nothing && return
    entry = """
    # time: $(Libc.strftime("%Y-%m-%d %H:%M:%S %Z", time()))
    # mode: $mode
    $(replace(str, r"^"ms, "\t"))
    """
    # TODO: write-lock history file
    seekend(hist.history_file)
    print(hist.history_file, entry)
    flush(hist.history_file)
end

function history_move(s::Union{LineEdit.MIState,LineEdit.PrefixSearchState}, hist::REPLHistoryProvider, idx::Int, save_idx::Int = hist.cur_idx)
    max_idx = length(hist.history) + 1
    @assert 1 <= hist.cur_idx <= max_idx
    (1 <= idx <= max_idx) || return :none
    idx != hist.cur_idx || return :none

    # save the current line
    if save_idx == max_idx
        hist.last_mode = LineEdit.mode(s)
        hist.last_buffer = copy(LineEdit.buffer(s))
    else
        hist.history[save_idx] = LineEdit.input_string(s)
        hist.modes[save_idx] = mode_idx(hist, LineEdit.mode(s))
    end

    # load the saved line
    if idx == max_idx
        LineEdit.transition(s, hist.last_mode) do
            LineEdit.replace_line(s, hist.last_buffer)
            hist.last_mode = nothing
            hist.last_buffer = IOBuffer()
        end
    else
        if haskey(hist.mode_mapping, hist.modes[idx])
            LineEdit.transition(s, hist.mode_mapping[hist.modes[idx]]) do
                LineEdit.replace_line(s, hist.history[idx])
            end
        else
            return :skip
        end
    end
    hist.cur_idx = idx

    return :ok
end

# Modified version of accept_result that also transitions modes
function LineEdit.accept_result(s, p::LineEdit.HistoryPrompt{REPLHistoryProvider})
    parent = LineEdit.state(s, p).parent
    hist = p.hp
    if 1 <= hist.cur_idx <= length(hist.modes)
        m = hist.mode_mapping[hist.modes[hist.cur_idx]]
        LineEdit.transition(s, m) do
            LineEdit.replace_line(LineEdit.state(s, m), LineEdit.state(s, p).response_buffer)
        end
    else
        LineEdit.transition(s, parent)
    end
end

function history_prev(s::LineEdit.MIState, hist::REPLHistoryProvider,
        save_idx::Int = hist.cur_idx)
    hist.last_idx = -1
    m = history_move(s, hist, hist.cur_idx-1, save_idx)
    if m == :ok
        LineEdit.move_input_start(s)
        LineEdit.reset_key_repeats(s) do
            LineEdit.move_line_end(s)
        end
        LineEdit.refresh_line(s)
    elseif m == :skip
        hist.cur_idx -= 1
        history_prev(s, hist, save_idx)
    else
        Terminals.beep(LineEdit.terminal(s))
    end
end

function history_next(s::LineEdit.MIState, hist::REPLHistoryProvider,
        save_idx::Int = hist.cur_idx)
    cur_idx = hist.cur_idx
    max_idx = length(hist.history) + 1
    if cur_idx == max_idx && 0 < hist.last_idx
        # issue #6312
        cur_idx = hist.last_idx
        hist.last_idx = -1
    end
    m = history_move(s, hist, cur_idx+1, save_idx)
    if m == :ok
        LineEdit.move_input_end(s)
        LineEdit.refresh_line(s)
    elseif m == :skip
        hist.cur_idx += 1
        history_next(s, hist, save_idx)
    else
        Terminals.beep(LineEdit.terminal(s))
    end
end

function history_move_prefix(s::LineEdit.PrefixSearchState,
                             hist::REPLHistoryProvider,
                             prefix::AbstractString,
                             backwards::Bool,
                             cur_idx = hist.cur_idx)
    cur_response = bytestring(LineEdit.buffer(s))
    # when searching forward, start at last_idx
    if !backwards && hist.last_idx > 0
        cur_idx = hist.last_idx
    end
    hist.last_idx = -1
    max_idx = length(hist.history)+1
    idxs = backwards ? ((cur_idx-1):-1:1) : ((cur_idx+1):max_idx)
    for idx in idxs
        if (idx == max_idx) || (startswith(hist.history[idx], prefix) && (hist.history[idx] != cur_response || hist.modes[idx] != LineEdit.mode(s)))
            m = history_move(s, hist, idx)
            if m == :ok
                if length(prefix) == 0
                    # on empty prefix search, move cursor to the end
                    LineEdit.move_input_end(s)
                else
                    # otherwise, keep cursor at the prefix position as a visual cue
                    seek(LineEdit.buffer(s), length(prefix))
                end
                LineEdit.refresh_line(s)
                return :ok
            elseif m == :skip
                return history_move_prefix(s,hist,prefix,backwards,idx)
            end
        end
    end
    Terminals.beep(LineEdit.terminal(s))
end
history_next_prefix(s::LineEdit.PrefixSearchState, hist::REPLHistoryProvider, prefix::AbstractString) =
    history_move_prefix(s, hist, prefix, false)
history_prev_prefix(s::LineEdit.PrefixSearchState, hist::REPLHistoryProvider, prefix::AbstractString) =
    history_move_prefix(s, hist, prefix, true)

function history_search(hist::REPLHistoryProvider, query_buffer::IOBuffer, response_buffer::IOBuffer,
                        backwards::Bool=false, skip_current::Bool=false)

    qpos = position(query_buffer)
    qpos > 0 || return true
    searchdata = bytestring_beforecursor(query_buffer)
    response_str = bytestring(response_buffer)

    # Alright, first try to see if the current match still works
    a = position(response_buffer) + 1
    b = a + sizeof(searchdata) - 1
    if !skip_current && (0 < a <= b <= response_buffer.size) &&
       searchdata == bytestring(response_buffer.data[a:b])
        return true
    end

    searchfunc,delta = backwards ? (rsearch,0) : (search,1)

    # Start searching
    # First the current response buffer
    if 1 <= a+delta <= length(response_str)
        match = searchfunc(response_str, searchdata, a+delta)
        if match != 0:-1
            seek(response_buffer, first(match)-1)
            return true
        end
    end

    # Now search all the other buffers
    idxs = backwards ? ((hist.cur_idx-1):-1:1) : ((hist.cur_idx+1):length(hist.history))
    for idx in idxs
        h = hist.history[idx]
        match = searchfunc(h, searchdata)
        if match != 0:-1 && h != response_str && haskey(hist.mode_mapping, hist.modes[idx])
            truncate(response_buffer, 0)
            write(response_buffer, h)
            seek(response_buffer, first(match)-1)
            hist.cur_idx = idx
            return true
        end
    end

    return false
end

function history_reset_state(hist::REPLHistoryProvider)
    hist.last_idx = hist.cur_idx
    hist.cur_idx = length(hist.history) + 1
end
LineEdit.reset_state(hist::REPLHistoryProvider) = history_reset_state(hist)

const julia_green = "\033[1m\033[32m"

function return_callback(s)
    ast = Base.syntax_deprecation_warnings(false) do
        Base.parse_input_line(bytestring(LineEdit.buffer(s)))
    end
    if  !isa(ast, Expr) || (ast.head != :continue && ast.head != :incomplete)
        return true
    else
        return false
    end
end

function find_hist_file()
    filename = ".julia_history"
    if isfile(filename)
        return filename
    elseif haskey(ENV, "JULIA_HISTORY")
        return ENV["JULIA_HISTORY"]
    else
        return joinpath(homedir(), filename)
    end
end

backend(r::AbstractREPL) = r.backendref

send_to_backend(ast, backend::REPLBackendRef) = send_to_backend(ast, backend.repl_channel, backend.response_channel)
function send_to_backend(ast, req, rep)
    put!(req, (ast, 1))
    val, bt = take!(rep)
end

function respond(f, repl, main; pass_empty = false)
    (s,buf,ok)->begin
        if !ok
            return transition(s, :abort)
        end
        line = takebuf_string(buf)
        if !isempty(line) || pass_empty
            reset(repl)
            val, bt = send_to_backend(f(line), backend(repl))
            if !ends_with_semicolon(line) || bt !== nothing
                print_response(repl, val, bt, true, Base.have_color)
            end
        end
        prepare_next(repl)
        reset_state(s)
        s.current_mode.sticky || transition(s, main)
    end
end

function reset(repl::LineEditREPL)
    raw!(repl.t, false)
    print(repl.t,Base.text_colors[:normal])
end

function prepare_next(repl::LineEditREPL)
    println(terminal(repl))
end

function mode_keymap(julia_prompt)
    AnyDict(
    '\b' => function (s,o...)
        if isempty(s) || position(LineEdit.buffer(s)) == 0
            buf = copy(LineEdit.buffer(s))
            transition(s, julia_prompt) do
                LineEdit.state(s, julia_prompt).input_buffer = buf
            end
        else
            LineEdit.edit_backspace(s)
        end
    end,
    "^C" => function (s,o...)
        LineEdit.move_input_end(s)
        LineEdit.refresh_line(s)
        print(LineEdit.terminal(s), "^C\n\n")
        transition(s, julia_prompt)
        transition(s, :reset)
        LineEdit.refresh_line(s)
    end)
end

function setup_interface(repl::LineEditREPL; hascolor = repl.hascolor, extra_repl_keymap = Dict{Any,Any}[])
    ###
    #
    # This function returns the main interface that describes the REPL
    # functionality, it is called internally by functions that setup a
    # Terminal-based REPL frontend, but if you want to customize your REPL
    # or embed the REPL in another interface, you may call this function
    # directly and append it to your interface.
    #
    # Usage:
    #
    # repl_channel,response_channel = Channel(),Channel()
    # start_repl_backend(repl_channel, response_channel)
    # setup_interface(REPLDisplay(t),repl_channel,response_channel)
    #
    ###

    ###
    # We setup the interface in two stages.
    # First, we set up all components (prompt,rsearch,shell,help)
    # Second, we create keymaps with appropriate transitions between them
    #   and assign them to the components
    #
    ###

    ############################### Stage I ################################

    # This will provide completions for REPL and help mode
    replc = REPLCompletionProvider(repl)

    # Set up the main Julia prompt
    julia_prompt = Prompt("julia> ";
        # Copy colors from the prompt object
        prompt_prefix = hascolor ? repl.prompt_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        keymap_func_data = repl,
        complete = replc,
        on_enter = return_callback)

    julia_prompt.on_done = respond(Base.parse_input_line, repl, julia_prompt)

    # Setup help mode
    help_mode = Prompt("help?> ",
        prompt_prefix = hascolor ? repl.help_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        keymap_func_data = repl,
        complete = replc,
        # When we're done transform the entered line into a call to help("$line")
        on_done = respond(repl, julia_prompt) do line
            line = strip(line)
            haskey(Docs.keywords, symbol(line)) ? # Special-case keywords, which won't parse
                :(Base.Docs.@repl $(symbol(line))) :
                Base.syntax_deprecation_warnings(false) do
                    parse("Base.Docs.@repl $line", raise=false)
                end
        end)

    # Set up shell mode
    shell_mode = Prompt("shell> ";
        prompt_prefix = hascolor ? repl.shell_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        keymap_func_data = repl,
        complete = ShellCompletionProvider(repl),
        # Transform "foo bar baz" into `foo bar baz` (shell quoting)
        # and pass into Base.repl_cmd for processing (handles `ls` and `cd`
        # special)
        on_done = respond(repl, julia_prompt) do line
            Expr(:call, :(Base.repl_cmd), macroexpand(Expr(:macrocall, symbol("@cmd"),line)), outstream(repl))
        end)

    ################################# Stage II #############################

    # Setup history
    # We will have a unified history for all REPL modes
    hp = REPLHistoryProvider(Dict{Symbol,Any}(:julia => julia_prompt,
                                              :shell => shell_mode,
                                              :help  => help_mode))
    if repl.history_file
        try
            f = open(find_hist_file(), true, true, true, false, false)
            finalizer(replc, replc->close(f))
            hist_from_file(hp, f)
        catch e
            print_response(repl, e, catch_backtrace(), true, Base.have_color)
            println(outstream(repl))
            info("Disabling history file for this session.")
            repl.history_file = false
        end
    end
    history_reset_state(hp)
    julia_prompt.hist = hp
    shell_mode.hist = hp
    help_mode.hist = hp

    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    search_prompt.complete = LatexCompletions()

    # Canonicalize user keymap input
    if isa(extra_repl_keymap, Dict)
        extra_repl_keymap = [extra_repl_keymap]
    end

    const repl_keymap = AnyDict(
        ';' => function (s,o...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, shell_mode) do
                    LineEdit.state(s, shell_mode).input_buffer = buf
                end
            else
                edit_insert(s, ';')
            end
        end,
        '?' => function (s,o...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, help_mode) do
                    LineEdit.state(s, help_mode).input_buffer = buf
                end
            else
                edit_insert(s, '?')
            end
        end,

        # Bracketed Paste Mode
        "\e[200~" => (s,o...)->begin
            input = LineEdit.bracketed_paste(s)
            buf = copy(LineEdit.buffer(s))
            edit_insert(buf, input)
            string = takebuf_string(buf)
            curspos = position(LineEdit.buffer(s))
            pos = 0
            inputsz = sizeof(input)
            sz = sizeof(string)
            while pos <= sz
                oldpos = pos
                ast, pos = Base.syntax_deprecation_warnings(false) do
                    Base.parse(string, pos, raise=false)
                end
                if isa(ast, Expr) && ast.head == :error
                    # Insert all the remaining text as one line (might be empty)
                    LineEdit.replace_line(s, strip(bytestring(string.data[max(oldpos, 1):end])))
                    seek(LineEdit.buffer(s), max(curspos-oldpos+inputsz, 0))
                    LineEdit.refresh_line(s)
                    break
                end
                # Get the line and strip leading and trailing whitespace
                line = strip(bytestring(string.data[max(oldpos, 1):min(pos-1, sz)]))
                isempty(line) && continue
                LineEdit.replace_line(s, line)
                if oldpos <= curspos
                    seek(LineEdit.buffer(s),curspos-oldpos+inputsz)
                end
                LineEdit.refresh_line(s)
                (pos > sz && last(string) != '\n') && break
                if !isa(ast, Expr) || (ast.head != :continue && ast.head != :incomplete)
                    LineEdit.commit_line(s)
                    # This is slightly ugly but ok for now
                    terminal = LineEdit.terminal(s)
                    raw!(terminal, false) && disable_bracketed_paste(terminal)
                    LineEdit.mode(s).on_done(s, LineEdit.buffer(s), true)
                    raw!(terminal, true) && enable_bracketed_paste(terminal)
                else
                    break
                end
            end
        end,
    )

    prefix_prompt, prefix_keymap = LineEdit.setup_prefix_keymap(hp, julia_prompt)

    a = Dict{Any,Any}[skeymap, repl_keymap, prefix_keymap, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    prepend!(a, extra_repl_keymap)

    julia_prompt.keymap_dict = LineEdit.keymap(a)

    mk = mode_keymap(julia_prompt)

    b = Dict{Any,Any}[skeymap, mk, prefix_keymap, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    prepend!(b, extra_repl_keymap)

    shell_mode.keymap_dict = help_mode.keymap_dict = LineEdit.keymap(b)

    ModalInterface([julia_prompt, shell_mode, help_mode, search_prompt, prefix_prompt])
end

function run_frontend(repl::LineEditREPL, backend)
    d = REPLDisplay(repl)
    dopushdisplay = repl.specialdisplay === nothing && !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    if !isdefined(repl,:interface)
        interface = repl.interface = setup_interface(repl)
    else
        interface = repl.interface
    end
    repl.backendref = backend
    run_interface(repl.t, interface)
    dopushdisplay && popdisplay(d)
end

if isdefined(Base, :banner_color)
    banner(io, t) = banner(io, hascolor(t))
    banner(io, x::Bool) = print(io, x ? Base.banner_color : Base.banner_plain)
else
    banner(io,t) = Base.banner(io)
end

## StreamREPL ##

type StreamREPL <: AbstractREPL
    stream::IO
    prompt_color::AbstractString
    input_color::AbstractString
    answer_color::AbstractString
    waserror::Bool
    StreamREPL(stream,pc,ic,ac) = new(stream,pc,ic,ac,false)
end

outstream(s::StreamREPL) = s.stream

StreamREPL(stream::IO) = StreamREPL(stream, julia_green, Base.text_colors[:white], Base.answer_color())

answer_color(r::LineEditREPL) = r.envcolors ? Base.answer_color() : r.answer_color
answer_color(r::StreamREPL) = r.answer_color
input_color(r::LineEditREPL) = r.envcolors ? Base.input_color() : r.input_color
input_color(r::StreamREPL) = r.input_color


function run_repl(stream::IO)
    repl =
    @async begin
        repl_channel = Channel(1)
        response_channel = Channel(1)
        start_repl_backend(repl_channel, response_channel)
        StreamREPL_frontend(repl, repl_channel, response_channel)
    end
    repl
end

function ends_with_semicolon(line)
    match = rsearch(line, ';')
    if match != 0
        for c in line[(match+1):end]
            isspace(c) || return c == '#'
        end
        return true
    end
    return false
end

function run_frontend(repl::StreamREPL, backend::REPLBackendRef)
    have_color = Base.have_color
    banner(repl.stream, have_color)
    d = REPLDisplay(repl)
    dopushdisplay = !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    repl_channel, response_channel = backend.repl_channel, backend.response_channel
    while repl.stream.open
        if have_color
            print(repl.stream,repl.prompt_color)
        end
        print(repl.stream, "julia> ")
        if have_color
            print(repl.stream, input_color(repl))
        end
        line = readline(repl.stream)
        if !isempty(line)
            ast = Base.parse_input_line(line)
            if have_color
                print(repl.stream, Base.color_normal)
            end
            put!(repl_channel, (ast, 1))
            val, bt = take!(response_channel)
            if !ends_with_semicolon(line)
                print_response(repl, val, bt, true, have_color)
            end
        end
    end
    # Terminate Backend
    put!(repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
end

function start_repl_server(port)
    listen(port) do server, status
        client = accept(server)
        run_repl(client)
    end
end

end # module
