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
}

have_color = false
color_answer = text_colors[:blue]
color_normal = text_colors[:normal]

banner() = print(have_color ? banner_color : banner_plain)

exit(n) = ccall(:jl_exit, Void, (Int32,), n)
exit() = exit(0)
quit() = exit()

function repl_callback(ast::ANY, show_value)
    global _repl_enough_stdin = true
    stop_reading(STDIN) 
    STDIN.readcb = false
    put(repl_channel, (ast, show_value))
end

# called to show a REPL result
repl_show(v::ANY) = repl_show(OUTPUT_STREAM, v)
function repl_show(io, v::ANY)
    if !(isa(v,Function) && isgeneric(v))
        if isa(v,AbstractVector) && !isa(v,Ranges)
            print(io, summary(v))
            if !isempty(v)
                println(io, ":")
                print_matrix(io, reshape(v,(length(v),1)))
            end
        else
            show(io, v)
        end
    end
    if isgeneric(v) && !isa(v,CompositeKind)
        show(io, v.env)
    end
end

function add_backtrace(e, bt)
    if isa(e,LoadError)
        if isa(e.error,LoadError)
            add_backtrace(e.error,bt)
        else
            e.error = BackTrace(e.error, bt)
            e
        end
    else
        BackTrace(e, bt)
    end
end

function eval_user_input(ast::ANY, show_value)
    iserr, lasterr, bt = false, (), nothing
    while true
        try
            if have_color
                print(color_normal)
            end
            if iserr
                show(add_backtrace(lasterr,bt))
                println()
                iserr, lasterr = false, ()
            else
                value = eval(Main,ast)
                global ans = value
                if !is(value,nothing) && show_value
                    if have_color
                        color = color_answer
                        try
                            key = symbol(lowercase(string(Main.ANSWER_COLOR)))
                            color = get(text_colors,key,color_answer)
                        end
                        print(color)
                    end
                    try repl_show(value)
                    catch err
                        throw(ShowError(value,err))
                    end
                    println()
                end
            end
            break
        catch err
            if iserr
                println("SYSTEM ERROR: show(lasterr) caused an error")
            end
            iserr, lasterr = true, err
            bt = backtrace()
        end
    end
    println()
end

function readBuffer(stream::AsyncStream, nread)
    global _repl_enough_stdin::Bool
    while !_repl_enough_stdin && nb_available(stream.buffer) > 0
        nread = int(memchr(stream.buffer,'\n')) # never more than one line or readline explodes :O
        nread2 = int(memchr(stream.buffer,'\r'))
        if nread == 0
            if nread2 == 0
                nread = nb_available(stream.buffer)
            else
                nread = nread2
            end
        else
            if nread2 != 0 && nread2 < nread
                nread = nread2
            end
        end
        ptr = pointer(stream.buffer.data,stream.buffer.ptr)
        skip(stream.buffer,nread)
        #println(STDERR,stream.buffer.data[stream.buffer.ptr-nread:stream.buffer.ptr-1])
        ccall(:jl_readBuffer,Void,(Ptr{Void},Int32),ptr,nread)
    end
    return false
end

function run_repl()
    global const repl_channel = RemoteRef()

    if have_color
        ccall(:jl_enable_color, Void, ())
    end

    # install Ctrl-C interrupt handler (InterruptException)
    ccall(:jl_install_sigint_handler, Void, ())
    STDIN.closecb = (x...)->put(repl_channel,(nothing,-1))

    while true
        ccall(:repl_callback_enable, Void, ())
        global _repl_enough_stdin = false
        start_reading(STDIN, readBuffer)
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
    println()
end

function parse_input_line(s::String)
    # s = bytestring(s)
    # (expr, pos) = parse(s, 1, true)
    # (ex, pos) = ccall(:jl_parse_string, Any,
    #                   (Ptr{Uint8},Int32,Int32),
    #                   s, int32(pos)-1, 1)
    # if !is(ex,())
    #     throw(ParseError("extra input after end of expression"))
    # end
    # expr
    ccall(:jl_parse_input_line, Any, (Ptr{Uint8},), s)
end

# try to include() a file, ignoring if not found
function try_include(f::String)
    if is_file_readable(f)
        include(f)
    end
end

function process_options(args::Array{Any,1})
    global ARGS
    quiet = false
    repl = true
    startup = true
    color_set = false
    i = 1
    while i <= length(args)
        if args[i]=="-q" || args[i]=="--quiet"
            quiet = true
        elseif args[i]=="--worker"
            start_worker()
            # doesn't return
        elseif args[i]=="-e"
            # TODO: support long options
            repl = false
            i+=1
            ARGS = args[i+1:end]
            eval(Main,parse_input_line(args[i]))
            break
        elseif args[i]=="-E"
            repl = false
            i+=1
            ARGS = args[i+1:end]
            show(eval(Main,parse_input_line(args[i])))
            println()
            break
        elseif args[i]=="-P"
            i+=1
            eval(Main,parse_input_line(args[i]))
        elseif args[i]=="-L"
            i+=1
            require(args[i])
        elseif args[i]=="-p"
            i+=1
            np = int32(args[i])
            addprocs_local(np-1)
        elseif args[i]=="--machinefile"
            i+=1
            machines = split(readall(args[i]), '\n', false)
            addprocs_ssh(machines)
        elseif args[i]=="-v" || args[i]=="--version"
            println("julia version ", VERSION)
            exit(0)
        elseif args[i]=="--no-history"
            # see repl-readline.c
        elseif args[i] == "-f" || args[i] == "--no-startup"
            startup = false
        elseif args[i] == "-F"
            # load juliarc now before processing any more options
            try_include(strcat(ENV["HOME"],"/.juliarc.jl"))
            startup = false
        elseif begins_with(args[i], "--color")
            if args[i] == "--color"
                color_set = true
                global have_color = true
            elseif args[i][8] == '='
                val = args[i][9:]
                if contains(("no","0","false"), val)
                    color_set = true
                    global have_color = false
                elseif contains(("yes","1","true"), val)
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
    return (quiet,repl,startup,color_set)
end

const roottask = current_task()
const roottask_wi = WorkItem(roottask)

is_interactive = false
isinteractive() = (is_interactive::Bool)

@windows_only const JULIA_USER_DATA_DIR = abspath(ENV["AppData"],"julia")

function _start()
    # set up standard streams

    @windows_only if !has(ENV,"HOME")
        ENV["HOME"] = joinpath(ENV["APPDATA"],"julia")
    end
    reinit_stdio()
    librandom_init()

    @windows_only if(!isdir(JULIA_USER_DATA_DIR))
        mkdir(JULIA_USER_DATA_DIR)
    end

    # set CPU core count
    global const CPU_CORES = ccall(:jl_cpu_cores, Int32, ())

    #atexit(()->flush(STDOUT))
    try
        global const Workqueue = WorkItem[]
        global const Waiting = Dict(64)

        if !anyp(a->(a=="--worker"), ARGS)
            # start in "head node" mode
            global const Scheduler = Task(()->event_loop(true), 1024*1024)
            global PGRP;
            PGRP.myid = 1
            assert(PGRP.np == 0)
            push!(PGRP.workers,LocalProcess())
            push!(PGRP.locs,("",0))
            PGRP.np = 1
            # make scheduler aware of current (root) task
            enq_work(roottask_wi)
            yield()
        end

        global const LOAD_PATH = ByteString[
            ".", # TODO: should we really look here?
            abspath(Pkg.dir()),
            abspath(JULIA_HOME,"..","share","julia","extras"),
        ]

        (quiet,repl,startup,color_set) = process_options(ARGS)

        if repl
            startup && try_include(joinpath(ENV["HOME"],".juliarc.jl"))

            if !color_set
                @unix_only global have_color = (begins_with(get(ENV,"TERM",""),"xterm") || success(`tput setaf 0`))
                @windows_only global have_color = true
            end

            global is_interactive = true
            quiet || banner()

            @unix_only    answer_color = "blue"
            @windows_only answer_color = "normal"
            if has(ENV,"JULIA_ANSWER_COLOR")
                answer_color = ENV["JULIA_ANSWER_COLOR"]
            elseif has(ENV,"JL_ANSWER_COLOR")
                warn("JL_ANSWER_COLOR is deprecated, use JULIA_ANSWER_COLOR instead.")
                answer_color = ENV["JL_ANSWER_COLOR"]
            end
            eval(Main,:(ANSWER_COLOR = $answer_color))
            global color_answer
            color_answer = get(text_colors,symbol(answer_color),color_answer)

            run_repl()
        end
    catch err
        show(add_backtrace(err,backtrace()))
        println()
        exit(1)
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
            show(err)
            println()
        end
    end
end

# Have colors passed as simple symbols: :black, :red, ...
function print_with_color(io::IO, color::Symbol, msg::String...)
    if have_color
        printed_color = get(text_colors, color, color_normal)
        print(io, printed_color, msg..., color_normal)
    else
        print(io, strs...)
    end
end
print_with_color(color::Symbol, msg::String...) =
    print_with_color(OUTPUT_STREAM, color, msg...)

# deprecated versions
function print_with_color(io::IO, msg::String, color::Symbol)
    warn("print_with_color(io, msg, color) is deprecated, ",
         "use print_with_color(io, color, msg) instead.")
    print_with_color(io, color, msg)
end
function print_with_color(msg::String, color::Symbol)
    warn("print_with_color(msg, color) is deprecated, ",
         "use print_with_color(color, msg) instead.")
    print_with_color(color, msg)
end

# Use colors to print messages and warnings in the REPL
info(msg::String...) = print_with_color(STDERR, :green, "MESSAGE: ", msg..., "\n")
warn(msg::String...) = print_with_color(STDERR, :red,   "WARNING: ", msg..., "\n")
