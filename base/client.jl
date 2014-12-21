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
        error("no cmd to execute")
    elseif cmd.exec[1] == "cd"
        new_oldpwd = pwd()
        if length(cmd.exec) > 2
            error("cd method only takes one argument")
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

function init_bind_addr(args::Vector{UTF8String})
    # Treat --bind-to in a position independent manner in ARGS since
    # --worker, -n and --machinefile options are affected by it
    btoidx = findfirst(args, "--bind-to")
    if btoidx > 0
        bind_to = split(args[btoidx+1], ":")
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


function process_options(args::Vector{UTF8String})
    quiet = false
    repl = true
    startup = true
    color_set = false
    no_history_file = false
    i = 1
    while i <= length(args)
        if args[i]=="-q" || args[i]=="--quiet"
            quiet = true
        elseif args[i]=="--worker"
            start_worker()
            # doesn't return
        elseif args[i]=="--bind-to"
            i+=1 # has already been processed
        elseif args[i]=="-e" || args[i]=="--eval"
            i == length(args) && error("-e,--eval  no <expr> provided")
            repl = false
            i+=1
            splice!(ARGS, 1:length(ARGS), args[i+1:end])
            eval(Main,parse_input_line(args[i]))
            break
        elseif args[i]=="-E" || args[i]=="--print"
            i == length(args) && error("-E,--print  no <expr> provided")
            repl = false
            i+=1
            splice!(ARGS, 1:length(ARGS), args[i+1:end])
            show(eval(Main,parse_input_line(args[i])))
            println()
            break
        elseif args[i]=="-P" || args[i]=="--post-boot"
            i == length(args) && error("-P,--post-boot  no <expr> provided")
            i+=1
            eval(Main,parse_input_line(args[i]))
        elseif args[i]=="-L" || args[i]=="--load"
            i == length(args) && error("-L, --load  no <file> provided")
            i+=1
            require(args[i])
        elseif args[i]=="-p"
            i == length(args) && error("-p  <n> processes not provided")
            i+=1
            if i > length(args) || !isdigit(args[i][1])
                np = Sys.CPU_CORES
                i -= 1
            else
                np = int(args[i])
                np < 1 && error("-p  <n> must be ≥ 1")
            end
            addprocs(np)
        elseif args[i]=="--machinefile"
            i == length(args) && error("--machinefile  no <file> provided")
            i+=1
            machines = load_machine_file(args[i])
            addprocs(machines)
        elseif args[i]=="-v" || args[i]=="--version"
            println("julia version ", VERSION)
            exit(0)
        elseif args[i]=="--no-history"
            # deprecated in v0.3
            warn("'--no-history' is deprecated; use '--no-history-file'")
            no_history_file = true
        elseif args[i] == "--no-history-file"
            no_history_file = true
        elseif args[i] == "-f" || args[i] == "--no-startup"
            startup = false
        elseif args[i] == "-F"
            # load juliarc now before processing any more options
            load_juliarc()
            startup = false
        elseif args[i] == "-i"
            global is_interactive = true
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
            if startup
                load_juliarc()
                startup = false
            end
            # program
            repl = false
            # remove julia's arguments
            splice!(ARGS, 1:length(ARGS), args[i+1:end])
            ccall(:jl_exit_on_sigint, Void, (Cint,), 1)
            include(args[i])
            break
        else
            error("unknown option: ", args[i])
        end
        i += 1
    end
    return (quiet,repl,startup,color_set,no_history_file)
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
    Sys.init_sysinfo()
    if CPU_CORES > 8 && !("OPENBLAS_NUM_THREADS" in keys(ENV)) && !("OMP_NUM_THREADS" in keys(ENV))
        # Prevent openblas from stating to many threads, unless/until specifically requested
        ENV["OPENBLAS_NUM_THREADS"] = 8
    end
end

function init_parallel()
    start_gc_msgs_task()
    atexit(terminate_all_workers)
end

import .Terminals
import .REPL

function _start()
    try
        init_parallel()
        init_bind_addr(ARGS)
        any(a->(a=="--worker"), ARGS) || init_head_sched()
        (quiet,repl,startup,color_set,no_history_file) = process_options(copy(ARGS))

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
                    active_repl.no_history_file = no_history_file
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
