## client.jl - frontend handling command line options, environment setup,
##             and REPL

const ARGS = UTF8String[]

const text_colors = AnyDict(
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
)

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

exit(n) = ccall(:jl_exit, Void, (Int32,), n)
exit() = exit(0)
quit() = exit()

function repl_cmd(cmd, out)
    shell = shell_split(get(ENV,"JULIA_SHELL",get(ENV,"SHELL","/bin/sh")))
    # Note that we can't support the fish shell due to its lack of subshells
    #   See this for details: https://github.com/JuliaLang/julia/issues/4918
    if Base.basename(shell[1]) == "fish"
        warn_once("cannot use the fish shell, defaulting to /bin/sh\
         set the JULIA_SHELL environment variable to silence this warning")
        shell = "/bin/sh"
    end

    if isempty(cmd.exec)
        throw(ArgumentError("no cmd to execute"))
    elseif cmd.exec[1] == "cd"
        new_oldpwd = pwd()
        if length(cmd.exec) > 2
            throw(ArgumentError("cd method only takes one argument"))
        elseif length(cmd.exec) == 2
            dir = cmd.exec[2]
            if dir == "-"
                if !haskey(ENV, "OLDPWD")
                    error("cd: OLDPWD not set")
                end
                cd(ENV["OLDPWD"])
            else
                cd(@windows? dir : readchomp(`$shell -c "echo $(shell_escape(dir))"`))
            end
        else
            cd()
        end
        ENV["OLDPWD"] = new_oldpwd
        println(out, pwd())
    else
        run(ignorestatus(@windows? cmd : (isa(STDIN, TTY) ? `$shell -i -c "($(shell_escape(cmd))) && true"` : `$shell -c "($(shell_escape(cmd))) && true"`)))
    end
    nothing
end

function repl_hook(input::AbstractString)
    Expr(:call, :(Base.repl_cmd),
         macroexpand(Expr(:macrocall,symbol("@cmd"),input)))
end

display_error(er) = display_error(er, [])
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
    stop_reading(STDIN)
    put!(repl_channel, (ast, show_value))
end

_repl_start = Condition()

syntax_deprecation_warnings(warn::Bool) =
    bool(ccall(:jl_parse_depwarn, Cint, (Cint,), warn))

function parse_input_line(s::AbstractString)
    # s = bytestring(s)
    # (expr, pos) = parse(s, 1)
    # (ex, pos) = ccall(:jl_parse_string, Any,
    #                   (Ptr{UInt8},Int32,Int32),
    #                   s, int32(pos)-1, 1)
    # if !is(ex,())
    #     throw(ParseError("extra input after end of expression"))
    # end
    # expr
    ccall(:jl_parse_input_line, Any, (Ptr{UInt8},), s)
end

function parse_input_line(io::IO)
    s = ""
    while !eof(io)
        s = s*readline(io)
        e = parse_input_line(s)
        if !(isa(e,Expr) && e.head === :incomplete)
            return e
        end
    end
end

# detect the reason which caused an :incomplete expression
# from the error message
# NOTE: the error messages are defined in src/julia-parser.scm
incomplete_tag(ex) = :none
function incomplete_tag(ex::Expr)
    Meta.isexpr(ex, :incomplete) || return :none
    msg = ex.args[1]
    contains(msg, "string") && return :string
    contains(msg, "comment") && return :comment
    contains(msg, "requires end") && return :block
    contains(msg, "\"`\"") && return :cmd
    contains(msg, "character") && return :char
    return :other
end

# try to include() a file, ignoring if not found
try_include(path::AbstractString) = isfile(path) && include(path)

# initialize the local proc network address / port
function init_bind_addr(opts::JLOptions)
    if opts.bindto != C_NULL
        bind_to = split(bytestring(opts.bindto), ":")
        bind_addr = string(parseip(bind_to[1]))
        if length(bind_to) > 1
            bind_port = parseint(bind_to[2])
        else
            bind_port = 0
        end
    else
        bind_port = 0
        try
            bind_addr = string(getipaddr())
        catch
            # All networking is unavailable, initialize bind_addr to the loopback address
            # Will cause an exception to be raised only when used.
            bind_addr = "127.0.0.1"
        end
    end
    global LPROC
    LPROC.bind_addr = bind_addr
    LPROC.bind_port = uint16(bind_port)
end

# NOTE: This set of required arguments need to be kept in sync with the required arguments defined in ui/repl.c
let reqarg = Set(UTF8String["--home",          "-H",
                            "--eval",          "-e",
                            "--print",         "-E",
                            "--post-boot",     "-P",
                            "--load",          "-L",
                            "--sysimage",      "-J",
                            "--cpu-target",    "-C",
                            "--procs",         "-p",
                            "--machinefile",
                            "--color",
                            "--history-file",
                            "--startup-file",
                            "--compile",
                            "--check-bounds",
                            "--int-literals",
                            "--dump-bitcode",
                            "--depwarn",
                            "--inline",
                            "--build",        "-b",
                            "--bind-to"])
    global process_options
    function process_options(opts::JLOptions, args::Vector{UTF8String})
        if !isempty(args) && (arg = first(args); arg[1] == '-' && in(arg, reqarg))
            println(STDERR, "julia: option `$arg` is missing an argument")
            exit(1)
        end
        repl = true
        startup = true
        history_file = true
        quiet = false
        color_set = false
        while true
            # show julia VERSION and quit
            if bool(opts.version)
                println(STDOUT, "julia version ", VERSION)
                exit(0)
            end
            # startup worker
            if bool(opts.worker)
                assert(opts.worker == 1 || opts.worker == 2)
                # if default, start worker process otherwise if custom pass through
                if opts.worker == 1
                    start_worker() # does not return
                end
            end
            # load file immediately on all processors
            if opts.load != C_NULL
                require(bytestring(opts.load))
            end
            # show banner
            quiet = bool(opts.quiet)
            # load ~/.juliarc file
            if opts.startupfile == 1
                load_juliarc()
                startup = false
            elseif opts.startupfile == 2
                startup = false
            end
            # load ~/.julia_history file
            history_file = bool(opts.historyfile)
            # add processors
            if opts.nprocs > 1
                addprocs(opts.nprocs)
            end
            # load processes from machine file
            if opts.machinefile != C_NULL
                addprocs(load_machine_file(bytestring(opts.machinefile)))
            end
            global is_interactive = bool(opts.isinteractive)
            # REPL color
            if opts.color == 0
                color_set = false
                global have_color = false
            elseif opts.color == 1
                color_set = true
                global have_color = true
            elseif opts.color == 2
                color_set = true
                global have_color = false
            end
            # eval expression
            if opts.eval != C_NULL
                repl = false
                eval(Main, parse_input_line(bytestring(opts.eval)))
                break
            end
            # eval expression and show result
            if opts.print != C_NULL
                repl = false
                show(eval(Main, parse_input_line(bytestring(opts.print))))
                println()
                break
            end
            # eval expression but don't disable interactive mode
            if opts.postboot != C_NULL
                eval(Main, parse_input_line(bytestring(opts.postboot)))
            end
            # load file
            if !isempty(args)
                if args[1][1] != '-'
                    if startup
                        load_juliarc()
                        startup = false
                    end
                    # program
                    repl = false
                    # remove filename from ARGS
                    shift!(ARGS)
                    ccall(:jl_exit_on_sigint, Void, (Cint,), 1)
                    include(args[1])
                else
                    println(STDERR, "julia: unknown option `$(args[1])`")
                    exit(1)
                end
            end
            break
        end
        return (quiet,repl,startup,color_set,history_file)
    end
end

const roottask = current_task()

is_interactive = false
isinteractive() = (is_interactive::Bool)

const LOAD_PATH = ByteString[]
function init_load_path()
    vers = "v$(VERSION.major).$(VERSION.minor)"
    if haskey(ENV,"JULIA_LOAD_PATH")
        prepend!(LOAD_PATH, split(ENV["JULIA_LOAD_PATH"], @windows? ';' : ':'))
    end
    push!(LOAD_PATH,abspath(JULIA_HOME,"..","local","share","julia","site",vers))
    push!(LOAD_PATH,abspath(JULIA_HOME,"..","share","julia","site",vers))
end

# start local process as head "master" process with process id  1
# register this process as a local worker
function init_head_sched()
    # start in "head node" mode
    global PGRP
    global LPROC
    LPROC.id = 1
    assert(length(PGRP.workers) == 0)
    register_worker(LPROC)
end

function load_juliarc()
    # If the user built us with a specific Base.SYSCONFDIR, check that location first for a juliarc.jl file
    #   If it is not found, then continue on to the relative path based on JULIA_HOME
    if !isempty(Base.SYSCONFDIR) && isfile(joinpath(JULIA_HOME,Base.SYSCONFDIR,"julia","juliarc.jl"))
        include(abspath(JULIA_HOME,Base.SYSCONFDIR,"julia","juliarc.jl"))
    else
        try_include(abspath(JULIA_HOME,"..","etc","julia","juliarc.jl"))
    end
    try_include(abspath(homedir(),".juliarc.jl"))
end

function load_machine_file(path::AbstractString)
    machines = []
    for line in split(readall(path),'\n'; keep=false)
        s = map!(strip, split(line,'*'; keep=false))
        if length(s) > 1
            cnt = isnumber(s[1]) ? int(s[1]) : symbol(s[1])
            push!(machines,(s[2], cnt))
        else
            push!(machines,line)
        end
    end
    return machines
end

function early_init()
    global const JULIA_HOME = ccall(:jl_get_julia_home, Any, ())
    # make sure OpenBLAS does not set CPU affinity (#1070, #9639)
    ENV["OPENBLAS_MAIN_FREE"] = get(ENV, "OPENBLAS_MAIN_FREE",
                                    get(ENV, "GOTOBLAS_MAIN_FREE", "1"))
    Sys.init_sysinfo()
    if CPU_CORES > 8 && !("OPENBLAS_NUM_THREADS" in keys(ENV)) && !("OMP_NUM_THREADS" in keys(ENV))
        # Prevent openblas from starting too many threads, unless/until specifically requested
        ENV["OPENBLAS_NUM_THREADS"] = 8
    end
end

# starts the gc message task (for distrubuted gc) and
# registers worker process termination method
function init_parallel()
    start_gc_msgs_task()
    atexit(terminate_all_workers)
end

import .Terminals
import .REPL

function _start()
    opts = JLOptions()
    try
        init_parallel()
        init_bind_addr(opts)
        # if this process is not a worker, schedule head process
        bool(opts.worker) || init_head_sched()
        (quiet,repl,startup,color_set,history_file) = process_options(opts,copy(ARGS))

        local term
        global active_repl
        if repl
            if !isa(STDIN,TTY)
                global is_interactive |= !isa(STDIN,Union(File,IOStream))
                color_set || (global have_color = false)
            else
                term = Terminals.TTYTerminal(get(ENV,"TERM",@windows? "" : "dumb"),STDIN,STDOUT,STDERR)
                global is_interactive = true
                color_set || (global have_color = Terminals.hascolor(term))
                quiet || REPL.banner(term,term)
                if term.term_type == "dumb"
                    active_repl = REPL.BasicREPL(term)
                    quiet || warn("Terminal not fully functional")
                else
                    active_repl = REPL.LineEditREPL(term, true)
                    active_repl.history_file = history_file
                    active_repl.hascolor = have_color
                end
                # Make sure any displays pushed in .juliarc.jl ends up above the
                # REPLDisplay
                pushdisplay(REPL.REPLDisplay(active_repl))
            end
        end

        startup && load_juliarc()

        if repl
            if !isa(STDIN,TTY)
                # note: currently IOStream is used for file STDIN
                if isa(STDIN,File) || isa(STDIN,IOStream)
                    # reading from a file, behave like include
                    eval(Main,parse_input_line(readall(STDIN)))
                else
                    # otherwise behave repl-like
                    while !eof(STDIN)
                        eval_user_input(parse_input_line(STDIN), true)
                    end
                end
            else
                REPL.run_repl(active_repl)
            end
        end
    catch err
        display_error(err,catch_backtrace())
        println()
        exit(1)
    end
    if is_interactive && have_color
        print(color_normal)
    end
end

const atexit_hooks = []

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
