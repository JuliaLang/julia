module REPL

using Base.Meta
using Base.Terminals
using Base.LineEdit
using Base.REPLCompletions

export StreamREPL, BasicREPL

import Base: AsyncStream,
             Display,
             display,
             writemime

import Base.Terminals: raw!

import Base.LineEdit:
    CompletionProvider,
    HistoryProvider,
    add_history,
    complete_line,
    history_next,
    history_next_prefix,
    history_prev,
    history_prev_prefix,
    history_search

abstract AbstractREPL

type REPLBackend
    repl_channel::RemoteRef
    response_channel::RemoteRef
    ans
end

function eval_user_input(ast::ANY, backend)
    iserr, lasterr, bt = false, (), nothing
    while true
        try
            if iserr
                put!(backend.response_channel, (lasterr, bt))
                iserr, lasterr = false, ()
            else
                ast = expand(ast)
                ans = Base.Meta.quot(backend.ans)
                eval(Main, :(ans = $(ans)))
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

function parse_input_line(s::String)
    # s = bytestring(s)
    # (expr, pos) = parse(s, 1)
    # (ex, pos) = ccall(:jl_parse_string, Any,
    #                   (Ptr{Uint8},Int32,Int32),
    #                   s, int32(pos)-1, 1)
    # if !is(ex,())
    #     throw(ParseError("extra input after end of expression"))
    # end
    # expr
    ccall(:jl_parse_input_line, Any, (Ptr{Uint8},), s)
end

function start_repl_backend(repl_channel, response_channel)
    backend = REPLBackend(repl_channel, response_channel, nothing)
    @async begin
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
end

function display_error(io::IO, er, bt)
    Base.with_output_color(:red, io) do io
        print(io, "ERROR: ")
        Base.showerror(io, er, bt)
    end
end

immutable REPLDisplay <: Display
    repl::AbstractREPL
end
function display(d::REPLDisplay, ::MIME"text/plain", x)
    io = outstream(d.repl)
    write(io, answer_color(d.repl))
    writemime(io, MIME("text/plain"), x)
    println(io)
end
display(d::REPLDisplay, x) = display(d, MIME("text/plain"), x)

function print_response(d::REPLDisplay, errio::IO, r::AbstractREPL, val::ANY, bt, show_value, have_color)
    while true
        try
            if bt !== nothing
                display_error(errio, val, bt)
                println(errio)
                iserr, lasterr = false, ()
            else
                if val !== nothing && show_value
                    try
                        display(d, val)
                    catch err
                        println(errio, "Error showing value of type ", typeof(val), ":")
                        rethrow(err)
                    end
                end
            end
            break
        catch err
            if bt !== nothing
                println(errio,"SYSTEM: show(lasterr) caused an error")
                break
            end
            val = err
            bt = catch_backtrace()
        end
    end
end
type LineEditREPL <: AbstractREPL
    t::TextTerminal
    prompt_color::String
    input_color::String
    answer_color::String
    shell_color::String
    help_color::String
    no_history_file::Bool
    in_shell::Bool
    in_help::Bool
    consecutive_returns::Int
end
outstream(r::LineEditREPL) = r.t

LineEditREPL(t::TextTerminal) =  LineEditREPL(t, julia_green,
                                              Base.input_color(),
                                              Base.answer_color(),
                                              Base.text_colors[:red],
                                              Base.text_colors[:yellow],
                                              false, false, false, 0)

type REPLCompletionProvider <: CompletionProvider
    r::LineEditREPL
end

type ShellCompletionProvider <: CompletionProvider
    r::LineEditREPL
end

function complete_line(c::REPLCompletionProvider, s)
    partial = bytestring(s.input_buffer.data[1:position(s.input_buffer)])
    ret, range, should_complete = completions(partial, endof(partial))
    return ret, partial[range], should_complete
end

function complete_line(c::ShellCompletionProvider, s)
    # First parse everything up to the current position
    partial = bytestring(s.input_buffer.data[1:position(s.input_buffer)])
    ret, range, should_complete = shell_completions(partial, endof(partial))
    return ret, partial[range], should_complete
end

type REPLHistoryProvider <: HistoryProvider
    history::Array{String,1}
    history_file
    cur_idx::Int
    last_idx::Int
    last_buffer::IOBuffer
    last_mode
    mode_mapping
    modes::Array{Uint8,1}
end
REPLHistoryProvider(mode_mapping) =
    REPLHistoryProvider(String[], nothing, 0, -1, IOBuffer(),
                        nothing, mode_mapping, Uint8[])

function hist_from_file(hp, file)
    hp.history_file = file
    seek(file, 0)
    while !eof(file)
        b = readuntil(file, '\0')
        if uint8(b[1]) in keys(hp.mode_mapping)
            push!(hp.modes, uint8(b[1]))
            push!(hp.history, b[2:(end-1)]) # Strip trailing \0
        else # For history backward compatibility
            push!(hp.modes, 0)
            push!(hp.history, b[1:(end-1)]) # Strip trailing \0
        end
    end
    seekend(file)
    hp
end

function mode_idx(hist::REPLHistoryProvider,mode)
    c::Uint8 = 0
    for (k,v) in hist.mode_mapping
        if k == uint8('\0')
            continue
        elseif v == mode
            c = k
            break
        end
    end
    @assert c != 0
    return c
end

function add_history(hist::REPLHistoryProvider, s)
    # bytestring copies
    str = bytestring(pointer(s.input_buffer.data), s.input_buffer.size)
    if isempty(strip(str)) || # Do not add empty strings to the history
       (length(hist.history) > 0 && str == hist.history[end]) # Do not add consecutive duplicate entries
        return
    end
    push!(hist.history, str)
    c = mode_idx(hist, LineEdit.mode(s))
    push!(hist.modes, c)
    if hist.history_file !== nothing
        write(hist.history_file, c)
        write(hist.history_file, str)
        write(hist.history_file, '\0')
        flush(hist.history_file)
    end
end

function history_move(s::LineEdit.MIState, hist::REPLHistoryProvider, idx::Int)
    max_idx = length(hist.history) + 1
    @assert 1 <= hist.cur_idx <= max_idx
    (1 <= idx <= max_idx) || return false
    idx != hist.cur_idx || return false

    # save the current line
    if hist.cur_idx == max_idx
        hist.last_mode = LineEdit.mode(s)
        hist.last_buffer = copy(LineEdit.buffer(s))
    else
        hist.history[hist.cur_idx] = LineEdit.input_string(s)
        hist.modes[hist.cur_idx] = mode_idx(hist, LineEdit.mode(s))
    end

    # load the saved line
    if idx == max_idx
        LineEdit.transition(s, hist.last_mode)
        LineEdit.replace_line(s, hist.last_buffer)
        hist.last_mode = nothing
        hist.last_buffer = IOBuffer()
    else
        LineEdit.transition(s, hist.mode_mapping[hist.modes[idx]])
        LineEdit.replace_line(s, hist.history[idx])
    end

    hist.cur_idx = idx
    true
end

function history_prev(s::LineEdit.MIState, hist::REPLHistoryProvider)
    hist.last_idx = -1
    if history_move(s, hist, hist.cur_idx-1)
        LineEdit.move_input_start(s)
        LineEdit.move_line_end(s)
    else
        Terminals.beep(LineEdit.terminal(s))
    end
end

function history_next(s::LineEdit.MIState, hist::REPLHistoryProvider)
    cur_idx = hist.cur_idx
    if 0 < hist.last_idx
        # issue #6312
        cur_idx = hist.last_idx
        hist.last_idx = -1
    end
    if history_move(s, hist, cur_idx+1)
        LineEdit.move_input_end(s)
    else
        Terminals.beep(LineEdit.terminal(s))
    end
end

function history_move_prefix(s::LineEdit.MIState,
                             hist::REPLHistoryProvider,
                             backwards::Bool)
    buf = LineEdit.buffer(s)
    n = buf.ptr - 1
    prefix = bytestring(buf.data[1:min(n,buf.size)])
    allbuf = bytestring(buf)
    idxs = backwards ? ((hist.cur_idx-1):-1:1) : ((hist.cur_idx+1):length(hist.history))
    for idx in idxs
        if beginswith(hist.history[idx], prefix) && hist.history[idx] != allbuf
            history_move(s, hist, idx)
            seek(LineEdit.buffer(s), n)
            LineEdit.refresh_line(s)
            return
        end
    end
    Terminals.beep(LineEdit.terminal(s))
end
history_next_prefix(s::LineEdit.MIState, hist::REPLHistoryProvider) =
    hist.cur_idx == length(hist.history) ?
        history_next(s, hist) : history_move_prefix(s, hist, false)
history_prev_prefix(s::LineEdit.MIState, hist::REPLHistoryProvider) =
    history_move_prefix(s, hist, true)

function history_search(hist::REPLHistoryProvider, query_buffer::IOBuffer, response_buffer::IOBuffer,
                        backwards::Bool=false, skip_current::Bool=false)
    if !(query_buffer.ptr > 1)
        #truncate(response_buffer,0)
        return true
    end

    # Alright, first try to see if the current match still works
    searchdata = bytestring(query_buffer.data[1:(query_buffer.ptr-1)])
    pos = position(response_buffer)
    if !skip_current && !((response_buffer.size == 0) || (pos+query_buffer.ptr-2 == 0)) &&
        (response_buffer.size >= (pos+query_buffer.ptr-2)) && (pos != 0) &&
        (searchdata == bytestring(response_buffer.data[pos:(pos+query_buffer.ptr-2)]))
        return true
    end

    # Start searching
    # First the current response buffer
    match = backwards ?
            rsearch(bytestring(response_buffer.data[1:response_buffer.size]), searchdata, response_buffer.ptr-1) :
            response_buffer.ptr + 1 < response_buffer.size ?
            search(bytestring(response_buffer.data[1:response_buffer.size]), searchdata, response_buffer.ptr+1) :
            0:-1

    if match != 0:-1
        seek(response_buffer, first(match)-1)
        return true
    end

    # Now search all the other buffers
    idx = hist.cur_idx
    found = false
    while true
        idx += backwards ? -1 : 1
        if !(0 < idx <= length(hist.history))
            break
        end
        match = backwards ? rsearch(hist.history[idx], searchdata):
                            search(hist.history[idx], searchdata);
        if match != 0:-1
            found = true
            truncate(response_buffer, 0)
            write(response_buffer, hist.history[idx])
            seek(response_buffer, first(match)-1)
            break
        end
    end
    if found
        #if hist.cur_idx == length(hist.history)+1
        #    hist.last_buffer = copy(s.input_buffer)
        #end
        hist.cur_idx = idx
    end
    return found
end

function history_reset_state(hist::REPLHistoryProvider)
    hist.last_idx = hist.cur_idx
    hist.cur_idx = length(hist.history) + 1
end
LineEdit.reset_state(hist::REPLHistoryProvider) = history_reset_state(hist)

const julia_green = "\033[1m\033[32m"

function return_callback(repl, s)
    if position(s.input_buffer) != 0 && eof(s.input_buffer) &&
        (seek(s.input_buffer, position(s.input_buffer)-1); read(s.input_buffer, Uint8) == '\n')
        repl.consecutive_returns += 1
    else
        repl.consecutive_returns = 0
    end
    ast = parse_input_line(bytestring(copy(s.input_buffer.data)))
    if repl.consecutive_returns > 1 || !isa(ast, Expr) || (ast.head != :continue && ast.head != :incomplete)
        return true
    else
        return false
    end
end

function find_hist_file()
    filename = ".julia_history2"
    if isfile(filename)
        return filename
    elseif haskey(ENV,"JULIA_HISTORY")
        return ENV["JULIA_HISTORY"]
    else
        return joinpath(homedir(), filename)
    end
end

function send_to_backend(ast, req, rep)
    put!(req, (ast, 1))
    val, bt = take!(rep)
end

have_color(s) = true

function respond(f, d, main, req, rep)
    (s,buf,ok)->begin
        if !ok
            return transition(s, :abort)
        end
        line = takebuf_string(buf)
        if !isempty(line)
            reset(d)
            val, bt = send_to_backend(f(line), req, rep)
            if !ends_with_semicolon(line) || bt !== nothing
                print_response(d, val, bt, true, have_color(s))
            end
        end
        println(d.repl.t)
        reset_state(s)
        transition(s, main)
    end
end

function reset(d::REPLDisplay)
    raw!(d.repl.t, false)
    print(Base.text_colors[:normal])
end



function setup_interface(d::REPLDisplay, req, rep; extra_repl_keymap = Dict{Any,Any}[])
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
    # repl_channel,response_channel = RemoteRef(),RemoteRef()
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

    repl = d.repl

    # This will provide completions for REPL and help mode
    replc = REPLCompletionProvider(repl)

    # Set up the main Julia prompt
    main_prompt = Prompt("julia> ";
        # Copy colors from the prompt object
        prompt_color = repl.prompt_color,
        input_color = repl.input_color,
        keymap_func_data = repl,
        complete = replc,
        on_enter = s->return_callback(repl, s))

    main_prompt.on_done = respond(Base.parse_input_line, d, main_prompt, req, rep)

    # Setup help mode
    help_mode = Prompt("help> ",
        prompt_color = repl.help_color,
        input_color = repl.input_color,
        keymap_func_data = repl,
        complete = replc,
        # When we're done transform the entered line into a call to help("$line")
        on_done = respond(d, main_prompt, req, rep) do line
            parse("Base.@help $line", raise=false)
        end)

    # Set up shell mode
    shell_mode = Prompt("shell> ";
        prompt_color = repl.shell_color,
        input_color = repl.input_color,
        keymap_func_data = repl,
        complete = ShellCompletionProvider(repl),
        # Transform "foo bar baz" into `foo bar baz` (shell quoting)
        # and pass into Base.repl_cmd for processing (handles `ls` and `cd`
        # special)
        on_done = respond(d, main_prompt, req, rep) do line
            Expr(:call, :(Base.repl_cmd), macroexpand(Expr(:macrocall, symbol("@cmd"),line)))
        end)

    ################################# Stage II #############################

    # Setup history
    # We will have a unified history for all REPL modes
    hp = REPLHistoryProvider((Uint8=>Any)[uint8('\0') => main_prompt,
                                          uint8(';') => shell_mode,
                                          uint8('?') => help_mode,
                                          uint8('>') => main_prompt])
    if !repl.no_history_file
        f = open(find_hist_file(), true, true, true, false, false)
        finalizer(replc, replc->close(f))
        hist_from_file(hp, f)
    end
    history_reset_state(hp)
    main_prompt.hist = hp
    shell_mode.hist = hp
    help_mode.hist = hp

    hkp, hkeymap = LineEdit.setup_search_keymap(hp)

    # Canonicalize user keymap input
    if isa(extra_repl_keymap, Dict)
        extra_repl_keymap = [extra_repl_keymap]
    end

    const repl_keymap = {
        ';' => function (s)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, shell_mode)
                LineEdit.state(s, shell_mode).input_buffer = buf
                LineEdit.refresh_line(s)
            else
                edit_insert(s, ';')
            end
        end,
        '?' => function (s)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, help_mode)
                LineEdit.state(s, help_mode).input_buffer = buf
                LineEdit.refresh_line(s)
            else
                edit_insert(s, '?')
            end
        end,

        # Bracketed Paste Mode
        "\e[200~" => s->begin
            ps = LineEdit.state(s, LineEdit.mode(s))
            input = readuntil(ps.terminal, "\e[201~")[1:(end-6)]
            input = replace(input, '\r', '\n')
            if position(LineEdit.buffer(s)) == 0
                indent = Base.indentation(input)[1]
                input = Base.unindent(lstrip(input), indent)
            end
            buf = copy(LineEdit.buffer(s))
            edit_insert(buf,input)
            string = takebuf_string(buf)
            pos = 0
            while pos <= length(string)
                oldpos = pos
                ast, pos = Base.parse(string, pos, raise=false)
                # Get the line and strip leading and trailing whitespace
                line = strip(string[max(oldpos, 1):min(pos-1, length(string))])
                isempty(line) && continue
                LineEdit.replace_line(s, line)
                LineEdit.refresh_line(s)
                (pos > length(string) && last(string) != '\n') && break
                if !isa(ast, Expr) || (ast.head != :continue && ast.head != :incomplete)
                    LineEdit.commit_line(s)
                    # This is slightly ugly but ok for now
                    terminal = LineEdit.terminal(s)
                    stop_reading(terminal)
                    raw!(terminal, false) && disable_bracketed_paste(terminal)
                    LineEdit.mode(s).on_done(s, LineEdit.buffer(s), true)
                    raw!(terminal, true) && enable_bracketed_paste(terminal)
                    start_reading(terminal)
                else
                    break
                end
            end
        end,
    }

    a = Dict{Any,Any}[hkeymap, repl_keymap, LineEdit.history_keymap(hp), LineEdit.default_keymap, LineEdit.escape_defaults]
    prepend!(a, extra_repl_keymap)
    @eval @LineEdit.keymap repl_keymap_func $(a)

    main_prompt.keymap_func = repl_keymap_func

    const mode_keymap = {
        '\b' => function (s)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, main_prompt)
                LineEdit.state(s, main_prompt).input_buffer = buf
                LineEdit.refresh_line(s)
            else
                LineEdit.edit_backspace(s)
            end
        end,
        "^C" => function (s)
            LineEdit.move_input_end(s)
            LineEdit.refresh_line(s)
            print(LineEdit.terminal(s), "^C\n\n")
            transition(s, main_prompt)
            transition(s, :reset)
            LineEdit.refresh_line(s)
        end
    }

    b = Dict{Any,Any}[hkeymap, mode_keymap, LineEdit.history_keymap(hp), LineEdit.default_keymap, LineEdit.escape_defaults]

    @eval @LineEdit.keymap mode_keymap_func $(b)

    shell_mode.keymap_func = help_mode.keymap_func = mode_keymap_func

    ModalInterface([main_prompt, shell_mode, help_mode,hkp])
end

run_frontend(repl::LineEditREPL, repl_channel, response_channel) =
    run_interface(repl.t, setup_interface(REPLDisplay(repl), repl_channel, response_channel))

if isdefined(Base, :banner_color)
    banner(io, t) = banner(io, hascolor(t))
    banner(io, x::Bool) = print(io, x ? Base.banner_color : Base.banner_plain)
else
    banner(io,t) = Base.banner(io)
end

function run_repl(repl::LineEditREPL)
    repl_channel = RemoteRef()
    response_channel = RemoteRef()
    start_repl_backend(repl_channel, response_channel)
    run_frontend(repl, repl_channel, response_channel)
end
run_repl(t::TextTerminal) = run_repl(LineEditREPL(t))

type BasicREPL <: AbstractREPL
end

outstream(::BasicREPL) = STDOUT

type StreamREPL <: AbstractREPL
    stream::IO
    prompt_color::String
    input_color::String
    answer_color::String
end

outstream(s::StreamREPL) = s.stream

StreamREPL(stream::AsyncStream) = StreamREPL(stream, julia_green, Base.text_colors[:white], Base.answer_color())

answer_color(r::LineEditREPL) = r.answer_color
answer_color(r::StreamREPL) = r.answer_color
answer_color(::BasicREPL) = Base.text_colors[:white]

print_response(d::REPLDisplay, r::StreamREPL,args...) = print_response(d, r.stream,r, args...)
print_response(d::REPLDisplay, r::LineEditREPL,args...) = print_response(d, r.t, r, args...)
print_response(d::REPLDisplay, args...) = print_response(d, d.repl, args...)

function run_repl(stream::AsyncStream)
    repl =
    @async begin
        repl_channel = RemoteRef()
        response_channel = RemoteRef()
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

function run_frontend(repl::StreamREPL, repl_channel, response_channel)
    have_color = true
    banner(repl.stream, have_color)
    d = REPLDisplay(repl)
    while repl.stream.open
        if have_color
            print(repl.stream,repl.prompt_color)
        end
        print(repl.stream, "julia> ")
        if have_color
            print(repl.stream, repl.input_color)
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
                print_response(d, val, bt, true, have_color)
            end
        end
    end
    # Terminate Backend
    put!(repl_channel, (nothing, -1))
end

function start_repl_server(port)
    listen(port) do server, status
        client = accept(server)
        run_repl(client)
    end
end

end # module
