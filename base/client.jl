# This file is a part of Julia. License is MIT: http://julialang.org/license

## client.jl - frontend handling command line options, environment setup,
##             and REPL

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

# Create a docstring with an automatically generated list
# of colors.
const possible_formatting_symbols = [:normal, :bold]
available_text_colors = collect(keys(text_colors))
available_text_colors =
    cat(1, intersect(available_text_colors, possible_formatting_symbols),
        sort(setdiff(  available_text_colors, possible_formatting_symbols)))

const available_text_colors_docstring =
    string(join([string("`:", key,"`")
                 for key in available_text_colors], ",\n", ", or \n"))

"""Dictionary of color codes for the terminal.

Available colors are: $available_text_colors_docstring.
"""
text_colors

have_color = false
default_color_warn = :red
default_color_info = :blue
@unix_only default_color_input = :bold
@unix_only default_color_answer = :bold
@windows_only default_color_input = :normal
@windows_only default_color_answer = :normal
color_normal = text_colors[:normal]

function repl_color(key, default)
    c = symbol(get(ENV, key, ""))
    haskey(text_colors, c) ? c : default
end

warn_color()   = repl_color("JULIA_WARN_COLOR", default_color_warn)
info_color()   = repl_color("JULIA_INFO_COLOR", default_color_info)
input_color()  = text_colors[repl_color("JULIA_INPUT_COLOR", default_color_input)]
answer_color() = text_colors[repl_color("JULIA_ANSWER_COLOR", default_color_answer)]

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

syntax_deprecation_warnings(warn::Bool) =
    ccall(:jl_parse_depwarn, Cint, (Cint,), warn) == 1

function syntax_deprecation_warnings(f::Function, warn::Bool)
    prev = syntax_deprecation_warnings(warn)
    try
        f()
    finally
        syntax_deprecation_warnings(prev)
    end
end

function parse_input_line(s::String)
    # (expr, pos) = parse(s, 1)
    # (ex, pos) = ccall(:jl_parse_string, Any,
    #                   (Ptr{UInt8},Csize_t,Int32,Int32),
    #                   s, sizeof(s), pos-1, 1)
    # if !is(ex,())
    #     throw(ParseError("extra input after end of expression"))
    # end
    # expr
    ccall(:jl_parse_input_line, Any, (Ptr{UInt8}, Csize_t), s, sizeof(s))
end
parse_input_line(s::AbstractString) = parse_input_line(bytestring(s))

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

function process_options(opts::JLOptions)
    if !isempty(ARGS)
        idxs = find(x -> x == "--", ARGS)
        if length(idxs) > 1
            println(STDERR, "julia: redundant option terminator `--`")
            exit(1)
        end
        deleteat!(ARGS, idxs)
    end
    repl                  = true
    startup               = (opts.startupfile != 2)
    history_file          = (opts.historyfile != 0)
    quiet                 = (opts.quiet != 0)
    color_set             = (opts.color != 0)
    global have_color     = (opts.color == 1)
    global is_interactive = (opts.isinteractive != 0)
    while true
        # load ~/.juliarc file
        startup && load_juliarc()

        # startup worker
        if opts.worker != 0
            start_worker() # does not return
        end
        # add processors
        if opts.nprocs > 0
            addprocs(opts.nprocs)
        end
        # load processes from machine file
        if opts.machinefile != C_NULL
            addprocs(load_machine_file(bytestring(opts.machinefile)))
        end
        # load file immediately on all processors
        if opts.load != C_NULL
            @sync for p in procs()
                @async remotecall_fetch(include, p, bytestring(opts.load))
            end
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
        if !isempty(ARGS) && !isempty(ARGS[1])
            # program
            repl = false
            # remove filename from ARGS
            global PROGRAM_FILE = String(shift!(ARGS))
            if !is_interactive
                ccall(:jl_exit_on_sigint, Void, (Cint,), 1)
            end
            include(PROGRAM_FILE)
        end
        break
    end
    repl |= is_interactive
    return (quiet,repl,startup,color_set,history_file)
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
    for line in split(readstring(path),'\n'; keep=false)
        s = map!(strip, split(line,'*'; keep=false))
        if length(s) > 1
            cnt = isnumber(s[1]) ? parse(Int,s[1]) : symbol(s[1])
            push!(machines,(s[2], cnt))
        else
            push!(machines,line)
        end
    end
    return machines
end

import .Terminals
import .REPL

const repl_hooks = []

atreplinit(f::Function) = (unshift!(repl_hooks, f); nothing)

function _atreplinit(repl)
    for f in repl_hooks
        try
            f(repl)
        catch err
            show(STDERR, err)
            println(STDERR)
        end
    end
end

function _start()
    empty!(ARGS)
    append!(ARGS, Core.ARGS)
    opts = JLOptions()
    try
        (quiet,repl,startup,color_set,history_file) = process_options(opts)

        local term
        global active_repl
        global active_repl_backend
        if repl
            if !isa(STDIN,TTY)
                global is_interactive |= !isa(STDIN,Union{File,IOStream})
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

        if repl
            if !isa(STDIN,TTY)
                # note: currently IOStream is used for file STDIN
                if isa(STDIN,File) || isa(STDIN,IOStream)
                    # reading from a file, behave like include
                    eval(Main,parse_input_line(readstring(STDIN)))
                else
                    # otherwise behave repl-like
                    while !eof(STDIN)
                        eval_user_input(parse_input_line(STDIN), true)
                    end
                end
            else
                _atreplinit(active_repl)
                REPL.run_repl(active_repl, backend->(global active_repl_backend = backend))
            end
        end
    catch err
        display_error(err,catch_backtrace())
        exit(1)
    end
    if is_interactive && have_color
        print(color_normal)
    end
end
