## client.jl - frontend handling command line options, environment setup,
##             and REPL

const text_colors = {
    :black   => "\033[1m\033[30m",
    :red     => "\033[1m\033[31m",
    :green   => "\033[1m\033[32m",
    :yellow  => "\033[1m\033[33m",
    :blue    => "\033[1m\033[34m",
    :magenta => "\033[1m\033[35m",
    :cyan    => "\033[1m\033[36m",
    :white   => "\033[1m\033[37m",
    :normal  => "\033[0m",
    :bold    => "\033[1m",
}

have_color = false
@unix_only default_color_answer = text_colors[:bold]
@unix_only default_color_input = text_colors[:bold]
@windows_only default_color_answer = text_colors[:normal]
@windows_only default_color_input = text_colors[:normal]
color_normal = text_colors[:normal]

function answer_color()
    c = symbol(get(ENV, "JULIA_ANSWER_COLOR", ""))
    return get(text_colors, c, default_color_answer)
end

function input_color()
    c = symbol(get(ENV, "JULIA_INPUT_COLOR", ""))
    return get(text_colors, c, default_color_input)
end

banner() = print(have_color ? banner_color : banner_plain)

exit(n) = ccall(:jl_exit, Void, (Int32,), n)
exit() = exit(0)
quit() = exit()

function repl_cmd(cmd)
    shell = shell_split(get(ENV,"JULIA_SHELL",get(ENV,"SHELL","/bin/sh")))
    # Note that we can't support the fish shell due to its lack of subshells
    #   See this for details: https://github.com/JuliaLang/julia/issues/4918
    if Base.basename(shell[1]) == "fish"
        warn_once("cannot use the fish shell, defaulting to /bin/sh\
         set the JULIA_SHELL environment variable to silence this warning")
        shell = "/bin/sh"
    end

    if isempty(cmd.exec)
        error("no cmd to execute")
    elseif cmd.exec[1] == "cd"
        if length(cmd.exec) > 2
            error("cd method only takes one argument")
        elseif length(cmd.exec) == 2
            dir = cmd.exec[2]
            cd(@windows? dir : readchomp(`$shell -c "echo $(shell_escape(dir))"`))
        else
            cd()
        end
        println(pwd())
    else
        run(@windows? cmd : `$shell -i -c "($(shell_escape(cmd))) && true"`)
    end
    nothing
end

function repl_hook(input::String)
    Expr(:call, :(Base.repl_cmd),
         macroexpand(Expr(:macrocall,symbol("@cmd"),input)))
end

function repl_methods(input::String)
    tokens = split(input, ".")
    fn = Main
    for token in tokens
        sym = symbol(token)
        isdefined(fn, sym) || return
        fn = fn.(sym)
    end
    isgeneric(fn) || return

    buf = IOBuffer()
    show_method_table(buf, methods(fn), -1, false)
    println(buf)
    takebuf_string(buf)
end

display_error(er) = display_error(er, {})
function display_error(er, bt)
    with_output_color(:red, STDERR) do io
        print(io, "ERROR: ")
        showerror(io, er, bt)
        println(io)
    end
end

function eval_user_input(ast::ANY, show_value)
    errcount, lasterr, bt = 0, (), nothing
    while true
        try
            if have_color
                print(color_normal)
            end
            if errcount > 0
                display_error(lasterr,bt)
                errcount, lasterr = 0, ()
            else
                ast = expand(ast)
                value = eval(Main,ast)
                eval(Main, :(ans = $(Expr(:quote, value))))
                if !is(value,nothing) && show_value
                    if have_color
                        print(answer_color())
                    end
                    try display(value)
                    catch err
                        println(STDERR, "Evaluation succeeded, but an error occurred while showing value of type ", typeof(value), ":")
                        rethrow(err)
                    end
                    println()
                end
            end
            break
        catch err
            if errcount > 0
                println(STDERR, "SYSTEM: show(lasterr) caused an error")
            end
            errcount, lasterr = errcount+1, err
            if errcount > 2
                println(STDERR, "WARNING: it is likely that something important is broken, and Julia will not be able to continue normally")
                break
            end
            bt = catch_backtrace()
        end
    end
    isa(STDIN,TTY) && println()
end

function repl_callback(ast::ANY, show_value)
    global _repl_enough_stdin = true
    put(repl_channel, (ast, show_value))
end

function run_repl()
    global const repl_channel = RemoteRef()

    ccall(:jl_init_repl, Void, (Cint,), _use_history)

    # install Ctrl-C interrupt handler (InterruptException)
    ccall(:jl_install_sigint_handler, Void, ())
    buf = Uint8[0]
    @async begin
        while !eof(STDIN)
            read(STDIN, buf)
            ccall(:jl_read_buffer,Void,(Ptr{Void},Cssize_t),buf,1)
            if _repl_enough_stdin
                yield()
            end
        end
        put(repl_channel,(nothing,-1))
    end

    while true
        if have_color
            prompt_string = "\01\033[1m\033[32m\02julia> \01\033[0m"*input_color()*"\02"
        else
            prompt_string = "julia> "
        end
        ccall(:repl_callback_enable, Void, (Ptr{Uint8},), prompt_string)
        global _repl_enough_stdin = false
        (ast, show_value) = take(repl_channel)
        if show_value == -1
            # exit flag
            break
        end
        eval_user_input(ast, show_value!=0)
    end

    if have_color
        print(color_normal)
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

function parse_input_line(io::IO)
    s = ""
    while !eof(io)
        s = s*readline(io)
        e = parse_input_line(s)
        if !(isa(e,Expr) && e.head === :continue)
            return e
        end
    end
end

# try to include() a file, ignoring if not found
try_include(path::String) = isfile(path) && include(path)

function process_options(args::Array{Any,1})
    global ARGS, bind_addr
    quiet = false
    repl = true
    startup = true
    color_set = false
    history = true
    i = 1
    while i <= length(args)
        if args[i]=="-q" || args[i]=="--quiet"
            quiet = true
        elseif args[i]=="--worker"
            start_worker()
            # doesn't return
        elseif args[i]=="--bind-to"
            i += 1
            bind_addr = args[i]
        elseif args[i]=="-e" || args[i]=="--eval"
            repl = false
            i+=1
            ARGS = args[i+1:end]
            eval(Main,parse_input_line(args[i]))
            break
        elseif args[i]=="-E" || args[i]=="--print"
            repl = false
            i+=1
            ARGS = args[i+1:end]
            show(eval(Main,parse_input_line(args[i])))
            println()
            break
        elseif args[i]=="-P" || args[i]=="--post-boot"
            i+=1
            eval(Main,parse_input_line(args[i]))
        elseif args[i]=="-L" || args[i]=="--load"
            i+=1
            require(args[i])
        elseif args[i]=="-p"
            i+=1
            if i > length(args) || !isdigit(args[i][1])
                np = Sys.CPU_CORES
                i -= 1
            else
                np = int(args[i])
            end
            addprocs(np)
        elseif args[i]=="--machinefile"
            i+=1
            machines = split(readall(args[i]), '\n', false)
            addprocs(machines)
        elseif args[i]=="-v" || args[i]=="--version"
            println("julia version ", VERSION)
            exit(0)
        elseif args[i]=="--no-history"
            history = false
        elseif args[i] == "-f" || args[i] == "--no-startup"
            startup = false
        elseif args[i] == "-F"
            # load juliarc now before processing any more options
            load_juliarc()
            startup = false
        elseif beginswith(args[i], "--color")
            if args[i] == "--color"
                color_set = true
                global have_color = true
            elseif args[i][8] == '='
                val = args[i][9:end]
                if in(val, ("no","0","false"))
                    color_set = true
                    global have_color = false
                elseif in(val, ("yes","1","true"))
                    color_set = true
                    global have_color = true
                end
            end
            if !color_set
                error("invalid option: ", args[i])
            end
        elseif args[i][1]!='-'
            # program
            repl = false
            # remove julia's arguments
            ARGS = args[i+1:end]
            include(args[i])
            break
        else
            error("unknown option: ", args[i])
        end
        i += 1
    end
    return (quiet,repl,startup,color_set,history)
end

const roottask = current_task()

is_interactive = false
isinteractive() = (is_interactive::Bool)

function init_load_path()
    vers = "v$(VERSION.major).$(VERSION.minor)"
    global const LOAD_PATH = ByteString[
        abspath(JULIA_HOME,"..","local","share","julia","site",vers),
        abspath(JULIA_HOME,"..","share","julia","site",vers)
    ]
end

function init_sched()
    global const Workqueue = Any[]
end

function init_head_sched()
    # start in "head node" mode
    global const Scheduler = Task(()->event_loop(true), 1024*1024)
    global PGRP
    global LPROC
    LPROC.id = 1
    assert(length(PGRP.workers) == 0)
    register_worker(LPROC)
    # make scheduler aware of current (root) task
    unshift!(Workqueue, roottask)
    yield()
end

function init_profiler()
    # Use a max size of 1M profile samples, and fire timer evey 1ms
    Profile.init(1_000_000, 0.001)
end

function load_juliarc()
    # If the user built us with a specifi Base.SYSCONFDIR, check that location first for a juliarc.jl file
    #   If it is not found, then continue on to the relative path based on JULIA_HOME
    if !isempty(Base.SYSCONFDIR) && isfile(joinpath(Base.SYSCONFDIR,"julia","juliarc.jl"))
        include(abspath(Base.SYSCONFDIR,"julia","juliarc.jl"))
    else
        try_include(abspath(JULIA_HOME,"..","etc","julia","juliarc.jl"))
    end
    try_include(abspath(homedir(),".juliarc.jl"))
end


function _start()
    # set up standard streams
    reinit_stdio()
    fdwatcher_reinit()
    # Initialize RNG
    Random.librandom_init()
    # Ensure PCRE is compatible with the compiled reg-exes
    PCRE.check_pcre()
    # Check that BLAS is correctly built
    check_blas()
    LinAlg.init()
    Sys.init()
    GMP.gmp_init()
    global const CPU_CORES = Sys.CPU_CORES
    init_profiler()

    @windows_only ENV["PATH"] = JULIA_HOME*";"*joinpath(JULIA_HOME,"..","Git","bin")*";"*ENV["PATH"]*
        ";C:\\Program Files\\Git\\bin;C:\\Program Files (x86)\\Git\\bin"*
        ";C:\\MinGW\\bin;C:\\MinGW\\msys\\1.0\\bin"*
        ";C:\\Python27;C:\\Python26;C:\\Python25"
    @windows_only haskey(ENV,"JULIA_EDITOR") || (ENV["JULIA_EDITOR"] = "start")
    @windows_only begin
        user_data_dir = abspath(ENV["AppData"],"julia")
        isdir(user_data_dir) || mkdir(user_data_dir)
    end

    #atexit(()->flush(STDOUT))
    try
        init_sched()
        any(a->(a=="--worker"), ARGS) || init_head_sched()
        init_load_path()
        (quiet,repl,startup,color_set,history) = process_options(ARGS)
        global _use_history = history
        repl && startup && load_juliarc()

        if repl
            if !isa(STDIN,TTY)
                if !color_set
                    global have_color = false
                end
                # note: currently IOStream is used for file STDIN
                if isa(STDIN,File) || isa(STDIN,IOStream)
                    # reading from a file, behave like include
                    global is_interactive = false
                    eval(parse_input_line(readall(STDIN)))
                else
                    # otherwise behave repl-like
                    global is_interactive = true
                    while !eof(STDIN)
                        eval_user_input(parse_input_line(STDIN), true)
                    end
                end
                if have_color
                    print(color_normal)
                end
                quit()
            end

            if !color_set
                @unix_only global have_color = (beginswith(get(ENV,"TERM",""),"xterm") || success(`tput setaf 0`))
                @windows_only global have_color = true
            end

            global is_interactive = true
            quiet || banner()

            if haskey(ENV,"JL_ANSWER_COLOR")
                warn("JL_ANSWER_COLOR is deprecated, use JULIA_ANSWER_COLOR instead.")
                ENV["JULIA_ANSWER_COLOR"] = ENV["JL_ANSWER_COLOR"]
            end

            run_repl()
        end
    catch err
        display_error(err,catch_backtrace())
        println()
        exit(1)
    end
    if is_interactive
        if have_color
            print(color_normal)
        end
        println()
    end
    ccall(:uv_atexit_hook, Void, ())
end

const atexit_hooks = {}

atexit(f::Function) = (unshift!(atexit_hooks, f); nothing)

function _atexit()
    for f in atexit_hooks
        try
            f()
        catch err
            show(STDERR, err)
            println(STDERR)
        end
    end
end
