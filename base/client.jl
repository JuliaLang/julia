## client.jl - frontend handling command line options, environment setup,
##             and REPL

@unix_only _jl_repl = _jl_lib
@windows_only _jl_repl = ccall(:jl_wrap_raw_dl_handle,Ptr{Void},(Ptr{Void},),ccall(:GetModuleHandleA,stdcall,Ptr{Void},(Ptr{Void},),C_NULL))

const _jl_color_normal = "\033[0m"

function _jl_answer_color()
    c = get(ENV, "JL_ANSWER_COLOR", "")
    return c == "black"   ? "\033[1m\033[30m" :
           c == "red"     ? "\033[1m\033[31m" :
           c == "green"   ? "\033[1m\033[32m" :
           c == "yellow"  ? "\033[1m\033[33m" :
           c == "magenta" ? "\033[1m\033[35m" :
           c == "cyan"    ? "\033[1m\033[36m" :
           c == "white"   ? "\033[1m\033[37m" :
           c == "normal"  ? _jl_color_normal  :
           "\033[1m\033[34m"
end


_jl_banner() = print(_jl_have_color ? _jl_banner_color : _jl_banner_plain)


exit(n) = ccall(:jl_exit, Void, (Int32,), n)
exit() = exit(0)
quit() = exit()

function repl_callback(ast::ANY, show_value)
    # use root task to execute user input
    stop_reading(STDIN)
    put(_jl_repl_channel, (ast, show_value))
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

function _jl_eval_user_input(ast::ANY, show_value)
    iserr, lasterr = false, ()
    while true
        try
            ccall(:jl_register_toplevel_eh, Void, ())
            ccall(:restore_signals, Void, ())
            if _jl_have_color
                print(_jl_color_normal)
            end
            if iserr
                show(lasterr)
                println()
                iserr, lasterr = false, ()
            else
                value = eval(ast)
                global ans = value
                if !is(value,nothing) && show_value
                    if _jl_have_color
                        print(_jl_answer_color())
                    end
                    try repl_show(value)
                    catch err
                        throw(ShowError(value,err))
                    end
                    println()
                end
            end
            break
        catch e
            if iserr
                println("SYSTEM ERROR: show(lasterr) caused an error")
            end
            iserr, lasterr = true, e
        end
    end
    println()
end

function readBuffer(stream::TTY)
    # This is bit tricky as :jl_readBuffer can trigger any behavior at any time (including running the event loop).
    # For now just reset the buffer before the call to it, though this may also have other unintended consequences.
    nread = stream.buffer.ptr-1
    stream.buffer.ptr = 1 #reuse buffer
    ccall(dlsym(_jl_repl,:jl_readBuffer),Void,(Ptr{Void},Int32),stream.buffer.data,nread)
    true
end

function run_repl()
    global const _jl_repl_channel = RemoteRef()

    if _jl_have_color
        ccall(dlsym(_jl_repl,:jl_enable_color), Void, ())
    end
    atexit() do
        if _jl_have_color
            print(_jl_color_normal)
        end
        println()
    end

    while true
        ccall(dlsym(_jl_repl,:repl_callback_enable), Void, ())
        STDIN.readcb = readBuffer
        start_reading(STDIN)
        (ast, show_value) = take(_jl_repl_channel)
        if show_value == -1
            # exit flag
            break
        end
        _jl_eval_user_input(ast, show_value!=0)
    end

    if iserr
        throw(lasterr)
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

function process_options(args::Array{Any,1})
    global ARGS
    # install Ctrl-C interrupt handler (InterruptException)
    ccall(:jl_install_sigint_handler, Void, ())
    quiet = false
    repl = true
    startup = true
    if has(ENV, "JL_POST_BOOT")
        eval(parse_input_line(ENV["JL_POST_BOOT"]))
    end
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
            eval(parse_input_line(args[i]))
            break
        elseif args[i]=="-E"
            repl = false
            i+=1
            ARGS = args[i+1:end]
            show(eval(parse_input_line(args[i])))
            println()
            break
        elseif args[i]=="-P"
            i+=1
            eval(parse_input_line(args[i]))
        elseif args[i]=="-L"
            i+=1
            load(args[i])
        elseif args[i]=="-p"
            i+=1
            np = int32(args[i])
            addprocs_local(np-1)
        elseif args[i]=="--machinefile"
            i+=1
            machines = split(readall(args[i]), '\n', false)
            addprocs_ssh(machines)
        elseif args[i]=="-v" || args[i]=="--version"
            println("julia version $VERSION")
            exit(0)
        elseif args[i]=="--no-history"
            # see repl-readline.c
        elseif args[i] == "-f" || args[i] == "--no-startup"
            startup = false
        elseif args[i][1]!='-'
            # program
            repl = false
            # remove julia's arguments
            ARGS = args[i+1:end]
            load(args[i])
            break
        else
            error("unknown option: ", args[i])
        end
        i += 1
    end
    return (quiet,repl,startup)
end

const _jl_roottask = current_task()
const _jl_roottask_wi = WorkItem(_jl_roottask)

_jl_is_interactive = false
isinteractive() = (_jl_is_interactive::Bool)

function _start()
    # set up standard streams
    reinit_stdio()

    librandom_init()

    # set CPU core count
    global const CPU_CORES = ccall(:jl_cpu_cores, Int32, ())

    #atexit(()->flush(stdout_stream))
    try
        ccall(:jl_register_toplevel_eh, Void, ())
        #ccall(:jl_start_io_thread, Void, ())
        global const Workqueue = WorkItem[]
        global const Waiting = Dict(64)

        if !anyp(a->(a=="--worker"), ARGS)
            # start in "head node" mode
            global const Scheduler = Task(()->event_loop(true), 1024*1024)
            global PGRP;
            PGRP.myid = 1
            assert(PGRP.np == 0)
            push(PGRP.workers,LocalProcess())
            push(PGRP.locs,Location("",0))
            PGRP.np = 1
            # make scheduler aware of current (root) task
            enq_work(_jl_roottask_wi)
            yield()
        end

        global const LOAD_PATH = String[
            ".",
            abs_path("$JULIA_HOME/../lib/julia"),
            abs_path("$JULIA_HOME/../lib/julia/base"),
            abs_path("$JULIA_HOME/../lib/julia/extras"),
            abs_path("$JULIA_HOME/../lib/julia/ui"),
        ]

        (quiet,repl,startup) = process_options(ARGS)

        if repl
            if startup
                try include(strcat(ENV["HOME"],"/.juliarc.jl")) end
            end

            @unix_only global _jl_have_color = begins_with(get(ENV,"TERM",""),"xterm") ||
                                    success(`tput setaf 0`)
            @windows_only global _jl_have_color = true
            global _jl_is_interactive = true
            if !quiet
                _jl_banner()
            end
            run_repl()
        end
    catch e
        show(e)
        println()
        exit(1)
    end
    exit(0) #HACK: always exit using jl_exit
end

const _jl_atexit_hooks = {}

atexit(f::Function) = (enqueue(_jl_atexit_hooks, f); nothing)

function _atexit()
    for f in _jl_atexit_hooks
        try
            f()
        catch e
            show(e)
            println()
        end
    end
end
