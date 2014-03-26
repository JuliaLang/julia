module REPL

    # compatibility for julia version 0.2
    if !isdefined(:put!)
        const put! = put
        const take! = take
    end

    export StreamREPL, BasicREPL

    abstract AbstractREPL

    type REPLBackend
        repl_channel::RemoteRef
        response_channel::RemoteRef
        ans
    end

    using Base.Meta

    function eval_user_input(ast::ANY, backend)
        iserr, lasterr, bt = false, (), nothing
        while true
            try
                if iserr
                    put!(backend.response_channel,(lasterr,bt))
                    iserr, lasterr = false, ()
                else
                    ast = expand(ast)
                    ans = Base.Meta.quot(backend.ans)
                    eval(Main,:(ans=$(ans)))
                    value = eval(Main,ast)
                    backend.ans = value
                    put!(backend.response_channel,(value,nothing))
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
        backend = REPLBackend(repl_channel,response_channel,nothing)
        @async begin
            # include looks at this to determine the relative include path
            # nothing means cwd
            while true
                tls = task_local_storage()
                tls[:SOURCE_PATH] = nothing
                (ast,show_value) = take!(backend.repl_channel)
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

    import Base: Display, display, writemime

    immutable REPLDisplay <: Display
        repl::AbstractREPL
    end
    function display(d::REPLDisplay, ::MIME"text/plain", x)
        io = outstream(d.repl)
        write(io,answer_color(d.repl))
        writemime(io, MIME("text/plain"), x)
        println(io)
    end
    display(d::REPLDisplay, x) = display(d, MIME("text/plain"), x)

    function print_response(d::REPLDisplay,errio::IO,r::AbstractREPL,val::ANY, bt, show_value, have_color)
        while true
            try
                if !is(bt,nothing)
                    display_error(errio,val,bt)
                    println(errio)
                    iserr, lasterr = false, ()
                else
                    if !is(val,nothing) && show_value
                        try display(d,val)
                        catch err
                            println(errio,"Error showing value of type ", typeof(val), ":")
                            rethrow(err)
                        end
                    end
                end
                break
            catch err
                if !is(bt,nothing)
                    println(errio,"SYSTEM: show(lasterr) caused an error")
                    break
                end
                val = err
                bt = catch_backtrace()
            end
        end
    end

    using Terminals
    using Readline

    import Readline: char_move_left, char_move_word_left, CompletionProvider, completeLine

    type ReadlineREPL <: AbstractREPL
        t::TextTerminal
        prompt_color::String
        input_color::String
        answer_color::String
        shell_color::String
        help_color::String
        in_shell::Bool
        in_help::Bool
        consecutive_returns
    end
    outstream(r::ReadlineREPL) = r.t

    ReadlineREPL(t::TextTerminal) =  ReadlineREPL(t,julia_green,Base.input_color(),Base.answer_color(),Base.text_colors[:red],Base.text_colors[:yellow],false,false,0)

    type REPLCompletionProvider <: CompletionProvider
        r::ReadlineREPL
    end

    type ShellCompletionProvider <: CompletionProvider
        r::ReadlineREPL
    end

    using REPLCompletions

    function completeLine(c::REPLCompletionProvider,s)
        partial = bytestring(s.input_buffer.data[1:position(s.input_buffer)])
        ret, range = completions(partial,endof(partial))
        return (ret,partial[range])
    end

    function completeLine(c::ShellCompletionProvider,s)
        # First parse everything up to the current position
        partial = bytestring(s.input_buffer.data[1:position(s.input_buffer)])
        ret, range = shell_completions(partial,endof(partial))
        return (ret, partial[range])
    end

    import Readline: HistoryProvider, add_history, history_prev, history_next, history_search

    type REPLHistoryProvider <: HistoryProvider
        history::Array{String,1}
        history_file
        cur_idx::Int
        last_buffer::IOBuffer
        last_mode
        mode_mapping
        modes::Array{Uint8,1}
    end

    function hist_from_file(file,mode_mapping)
        hp = REPLHistoryProvider(String[],file,0,IOBuffer(),nothing,mode_mapping,Uint8[])
        seek(file,0)
        while !eof(file)
            b = readuntil(file,'\0')
            if uint8(b[1]) in keys(mode_mapping)
                push!(hp.modes,uint8(b[1]))
                push!(hp.history,b[2:(end-1)]) # Strip trailing \0
            else # For history backward compatibility 
                push!(hp.modes,0)
                push!(hp.history,b[1:(end-1)]) # Strip trailing \0
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

    function add_history(hist::REPLHistoryProvider,s)
        # bytestring copies
        str = bytestring(pointer(s.input_buffer.data),s.input_buffer.size)
        if isempty(strip(str)) # Do not add empty strings to the history
            return
        end
        push!(hist.history,str)
        c = mode_idx(hist,Readline.mode(s))
        push!(hist.modes,c)
        write(hist.history_file,c)
        write(hist.history_file,str)
        write(hist.history_file,'\0')
        flush(hist.history_file)
    end

    function history_adjust(hist::REPLHistoryProvider,s)
        if 0 < hist.cur_idx <= length(hist.history)
            hist.history[hist.cur_idx] = Readline.input_string(s)
            hist.modes[hist.cur_idx] = mode_idx(hist,Readline.mode(s))
        end
    end

    function history_prev(s::Readline.MIState,hist::REPLHistoryProvider)
        if hist.cur_idx > 1
            if hist.cur_idx == length(hist.history)+1
                hist.last_mode = Readline.mode(s)
                hist.last_buffer = copy(Readline.buffer(s))
            else
                history_adjust(hist,s)
            end
            hist.cur_idx-=1
            Readline.transition(s,hist.mode_mapping[hist.modes[hist.cur_idx]])
            Readline.replace_line(s,hist.history[hist.cur_idx])
            Readline.refresh_line(s)
        else
            Terminals.beep(Readline.terminal(s))
        end
    end

    function history_next(s::Readline.MIState,hist::REPLHistoryProvider)
        if hist.cur_idx < length(hist.history)
            history_adjust(hist,s)
            hist.cur_idx+=1
            Readline.transition(s,hist.mode_mapping[hist.modes[hist.cur_idx]])
            Readline.replace_line(s,hist.history[hist.cur_idx])
            Readline.refresh_line(s)
        elseif hist.cur_idx == length(hist.history)
            hist.cur_idx+=1
            buf = hist.last_buffer
            hist.last_buffer = IOBuffer()
            Readline.transition(s,hist.last_mode)
            Readline.replace_line(s,buf)
            Readline.refresh_line(s)
        else
            Terminals.beep(Readline.terminal(s))
        end
    end

    function history_search(hist::REPLHistoryProvider,query_buffer::IOBuffer,response_buffer::IOBuffer,backwards::Bool=false, skip_current::Bool=false)
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
                rsearch(bytestring(response_buffer.data[1:response_buffer.size]),searchdata,response_buffer.ptr - 1):
                response_buffer.ptr + 1 < response_buffer.size ? 
                search(bytestring(response_buffer.data[1:response_buffer.size]),searchdata,response_buffer.ptr + 1): 0:-1

        if match != 0:-1
            seek(response_buffer,first(match)-1)
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
            match = backwards ? rsearch(hist.history[idx],searchdata):
                                search(hist.history[idx],searchdata);
            if match != 0:-1
                found = true
                truncate(response_buffer,0)
                write(response_buffer,hist.history[idx])
                seek(response_buffer,first(match)-1)
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
        hist.cur_idx = length(hist.history)+1
    end
    Readline.reset_state(hist::REPLHistoryProvider) = history_reset_state(hist)

    const julia_green = "\033[1m\033[32m"
    const color_normal = Base.color_normal

    function return_callback(repl,s)
        if position(s.input_buffer) != 0 && eof(s.input_buffer) && 
            (seek(s.input_buffer,position(s.input_buffer)-1);read(s.input_buffer,Uint8)=='\n')
            repl.consecutive_returns += 1
        else
            repl.consecutive_returns = 0
        end
        ast = parse_input_line(bytestring(copy(s.input_buffer.data)))
        if repl.consecutive_returns > 1 || !isa(ast,Expr) || (ast.head != :continue && ast.head != :incomplete)
            return true
        else 
            return false
        end
    end

    function find_hist_file()
        if isfile(".julia_history2")
            return ".julia_history2"
        elseif haskey(ENV,"JULIA_HISTORY")
            return ENV["JULIA_HISTORY"]
        else
            @windows_only return ENV["AppData"]*"/julia/history2"
            @unix_only return ENV["HOME"]*"/.julia_history2" 
        end
    end

    function send_to_backend(ast,req,rep)
        put!(req, (ast,1))
        (val, bt) = take!(rep)
    end

    have_color(s) = true

    function respond(f,d,main,req,rep)
        (s,buf,ok)->begin
            if !ok
                return transition(s,:abort)
            end
            line = takebuf_string(buf)
            if !isempty(line)
                reset(d)
                (val,bt) = send_to_backend(f(line),req,rep)
                if !ends_with_semicolon(line)
                    print_response(d,val,bt,true,have_color(s))
                end
            end
            println(d.repl.t)
            reset_state(s)
            transition(s,main)
        end
    end

    import Terminals: raw!

    function reset(d::REPLDisplay)
        raw!(d.repl.t,false)
        print(Base.text_colors[:normal])
    end



    function setup_interface(d::REPLDisplay,req,rep;extra_repl_keymap=Dict{Any,Any}[])
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
        finalizer(replc,(replc)->close(f))

        # Set up the main Julia prompt
        main_prompt = Prompt("julia> ";
            # Copy colors from the prompt object
            prompt_color=repl.prompt_color,
            input_color=repl.input_color,
            keymap_func_data = repl,
            complete=replc,
            on_enter=s->return_callback(repl,s))

        main_prompt.on_done = respond(Base.parse_input_line,d,main_prompt,req,rep)

        # Setup help mode
        help_mode = Prompt("help> ",
            prompt_color = repl.help_color,
            input_color=repl.input_color,
            keymap_func_data = repl,
            complete = replc,
            # When we're done transform the entered line into a call to help("$line")
            on_done = respond(d,main_prompt,req,rep) do line
                parse("Base.@help $line")
            end)

        # Set up shell mode
        shell_mode = Prompt("shell> ";
            prompt_color = repl.shell_color,
            input_color=repl.input_color,
            keymap_func_data = repl,
            complete = ShellCompletionProvider(repl),
            # Transform "foo bar baz" into `foo bar baz` (shell quoting)
            # and pass into Base.repl_cmd for processing (handles `ls` and `cd`
            # special)
            on_done = respond(d,main_prompt,req,rep) do line
                Expr(:call, :(Base.repl_cmd), macroexpand(Expr(:macrocall,symbol("@cmd"),line)))
            end)

        ################################# Stage II #############################

        # Setup history 
        # We will have a unified history for all REPL modes
        f = open(find_hist_file(),true,true,true,false,false)
        hp = hist_from_file(f,(Uint8=>Any)[uint8('\0') => main_prompt, uint8(';') => shell_mode, uint8('?') => help_mode, uint8('>') => main_prompt])
        history_reset_state(hp)
        main_prompt.hist = hp
        shell_mode.hist = hp
        help_mode.hist = hp

        (hkp,hkeymap) = Readline.setup_search_keymap(hp)

        # Canoniczlize user keymap input
        if isa(extra_repl_keymap,Dict)
            extra_repl_keymap = [extra_repl_keymap]
        end


        const repl_keymap = {
            ';' => function (s)
                if isempty(s) || position(Readline.buffer(s)) == 0
                    buf = copy(Readline.buffer(s))
                    transition(s,shell_mode)
                    Readline.state(s,shell_mode).input_buffer = buf
                    Readline.refresh_line(s)
                else
                    edit_insert(s,';')
                end
            end,
            '?' => function (s)
                if isempty(s) || position(Readline.buffer(s)) == 0
                    buf = copy(Readline.buffer(s))
                    transition(s,help_mode)
                    Readline.state(s,help_mode).input_buffer = buf
                    Readline.refresh_line(s)
                else
                    edit_insert(s,'?')
                end
            end,

            # Bracketed Paste Mode
            "\e[200~" => s->begin
                ps = Readline.state(s,Readline.mode(s))
                input = readuntil(ps.terminal,"\e[201~")[1:(end-6)]
                input = replace(input,'\r','\n')
                if position(Readline.buffer(s)) == 0
                    indent = Base.indentation(input)[1]
                    input = Base.unindent(input[(indent+1):end],indent)
                end
                buf = copy(Readline.buffer(s))
                edit_insert(buf,input)
                string = takebuf_string(buf)
                pos = 0
                while pos <= length(string)
                    oldpos = pos
                    ast, pos = Base.parse(string,pos; )
                    # Get the line and strip leading and trailing whitespace
                    line = strip(string[max(oldpos,1):min(pos-1,length(string))])
                    Readline.replace_line(s,line)
                    Readline.refresh_line(s)
                    if !isa(ast,Expr) || (ast.head != :continue && ast.head != :incomplete)
                        Readline.commit_line(s)
                        # This is slightly ugly but ok for now
                        terminal = Readline.terminal(s)
                        stop_reading(terminal)
                        raw!(terminal,false) && Terminals.Unix.disable_bracketed_paste(terminal)
                        Readline.mode(s).on_done(s,Readline.buffer(s),true)
                        raw!(terminal,true) && Terminals.Unix.enable_bracketed_paste(terminal)
                        start_reading(terminal)
                    else
                        break
                    end
                end
            end,
        }

        a = Dict{Any,Any}[hkeymap, repl_keymap, Readline.history_keymap(hp), Readline.default_keymap,Readline.escape_defaults]
        prepend!(a,extra_repl_keymap)
        @eval @Readline.keymap repl_keymap_func $(a)

        main_prompt.keymap_func = repl_keymap_func

        const mode_keymap = {
            '\b' => s->(isempty(s) ? transition(s,main_prompt) : Readline.edit_backspace(s) )
        }

        b = Dict{Any,Any}[hkeymap, mode_keymap, Readline.history_keymap(hp), Readline.default_keymap,Readline.escape_defaults]

        @eval @Readline.keymap mode_keymap_func $(b)

        shell_mode.keymap_func = help_mode.keymap_func = mode_keymap_func

        ModalInterface([main_prompt,shell_mode,help_mode,hkp])
    end

    run_frontend(repl::ReadlineREPL,repl_channel,response_channel) = run_interface(repl.t,setup_interface(REPLDisplay(repl),repl_channel,response_channel))

    if isdefined(Base,:banner_color)
        banner(io,t) = banner(io,hascolor(t))
        banner(io,x::Bool) = print(io,x ? Base.banner_color : Base.banner_plain)
    else
        banner(io,t) = Base.banner(io)
    end

    function run_repl(t::TextTerminal)
        repl_channel = RemoteRef()
        response_channel = RemoteRef()
        start_repl_backend(repl_channel, response_channel)
        banner(t,t)
        run_frontend(ReadlineREPL(t),repl_channel,response_channel)
    end

    type BasicREPL <: AbstractREPL
    end

    outstream(::BasicREPL) = STDOUT

    type StreamREPL <: AbstractREPL
        stream::IO
        prompt_color::String
        input_color::String
        answer_color::String
    end

    import Base.AsyncStream

    outstream(s::StreamREPL) = s.stream

    StreamREPL(stream::AsyncStream) = StreamREPL(stream,julia_green,Base.text_colors[:white],Base.answer_color())

    answer_color(r::ReadlineREPL) = r.answer_color
    answer_color(r::StreamREPL) = r.answer_color
    answer_color(::BasicREPL) = Base.text_colors[:white]

    print_response(d::REPLDisplay,r::StreamREPL,args...) = print_response(d, r.stream,r, args...)
    print_response(d::REPLDisplay,r::ReadlineREPL,args...) = print_response(d, r.t, r, args...)
    print_response(d::REPLDisplay,args...) = print_response(d,d.repl,args...)

    function run_repl(stream::AsyncStream)
    repl = 
    @async begin
        repl_channel = RemoteRef()
        response_channel = RemoteRef()
        start_repl_backend(repl_channel,response_channel)
        StreamREPL_frontend(repl,repl_channel,response_channel)
    end
    repl
    end

    function ends_with_semicolon(line)
        match = rsearch(line,';') 
        if match != 0
            for c in line[(match+1):end]
                if !isspace(c)
                    return c == '#'
                end
            end
            return true
        end
        return false
    end

    function run_frontend(repl::StreamREPL,repl_channel,response_channel)
        have_color = true
        banner(repl.stream,have_color)
        d = REPLDisplay(repl)
        while repl.stream.open
            if have_color
                print(repl.stream,repl.prompt_color)
            end
            print(repl.stream,"julia> ")
            if have_color
                print(repl.stream,repl.input_color)
            end
            line = readline(repl.stream)
            if !isempty(line)
                ast = Base.parse_input_line(line)
                if have_color
                    print(repl.stream,color_normal)
                end
                put!(repl_channel, (ast,1))
                (val, bt) = take!(response_channel)
                if !ends_with_semicolon(line)
                    print_response(d,val,bt,true,have_color)
                end
            end
        end
        # Terminate Backend
        put!(repl_channel,(nothing,-1))    
    end

    function start_repl_server(port)
        listen(port) do server, status
            client = accept(server)
            run_repl(client)
        end
    end
end
