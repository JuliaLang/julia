# This file is a part of Julia. License is MIT: http://julialang.org/license

## client.jl - frontend handling command line options, environment setup,
##             and REPL

const text_colors = AnyDict(
    :black         => "\033[30m",
    :red           => "\033[31m",
    :green         => "\033[32m",
    :yellow        => "\033[33m",
    :blue          => "\033[34m",
    :magenta       => "\033[35m",
    :cyan          => "\033[36m",
    :white         => "\033[37m",
    :light_black   => "\033[90m", # gray
    :light_red     => "\033[91m",
    :light_green   => "\033[92m",
    :light_yellow  => "\033[93m",
    :light_blue    => "\033[94m",
    :light_magenta => "\033[95m",
    :light_cyan    => "\033[96m",
    :normal        => "\033[0m",
    :default       => "\033[39m",
    :bold          => "\033[1m",
    :nothing       => "",
)

for i in 0:255
    text_colors[i] = "\033[38;5;$(i)m"
end

const disable_text_style = AnyDict(
    :bold => "\033[22m",
    :normal => "",
    :default => "",
)

# Create a docstring with an automatically generated list
# of colors.
available_text_colors = collect(Iterators.filter(x -> !isa(x, Integer), keys(text_colors)))
const possible_formatting_symbols = [:normal, :bold, :default]
available_text_colors = cat(1,
    sort!(intersect(available_text_colors, possible_formatting_symbols), rev=true),
    sort!(setdiff(  available_text_colors, possible_formatting_symbols)))

const available_text_colors_docstring =
    string(join([string("`:", key,"`")
                 for key in available_text_colors], ",\n", ", or \n"))

"""Dictionary of color codes for the terminal.

Available colors are: $available_text_colors_docstring as well as the integers 0 to 255 inclusive.

The color `:default` will print text in the default color while the color `:normal`
will print text with all text properties (like boldness) reset.
Printing with the color `:nothing` will print the string without modifications.
"""
text_colors

have_color = false
default_color_warn = :yellow
default_color_error = :light_red
default_color_info = :cyan
default_color_input = :normal
default_color_answer = :normal
color_normal = text_colors[:normal]

function repl_color(key, default)
    env_str = get(ENV, key, "")
    c = tryparse(Int, env_str)
    c_conv = isnull(c) ? Symbol(env_str) : get(c)
    haskey(text_colors, c_conv) ? c_conv : default
end

error_color() = repl_color("JULIA_ERROR_COLOR", default_color_error)
warn_color()  = repl_color("JULIA_WARN_COLOR" , default_color_warn)
info_color()  = repl_color("JULIA_INFO_COLOR" , default_color_info)

input_color()  = text_colors[repl_color("JULIA_INPUT_COLOR", default_color_input)]
answer_color() = text_colors[repl_color("JULIA_ANSWER_COLOR", default_color_answer)]

stackframe_lineinfo_color() = repl_color("JULIA_STACKFRAME_LINEINFO_COLOR", :bold)
stackframe_function_color() = repl_color("JULIA_STACKFRAME_FUNCTION_COLOR", :bold)

function repl_cmd(cmd, out)
    shell = shell_split(get(ENV,"JULIA_SHELL",get(ENV,"SHELL","/bin/sh")))
    shell_name = Base.basename(shell[1])

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
                cd(@static is_windows() ? dir : readchomp(`$shell -c "echo $(shell_escape(dir))"`))
            end
        else
            cd()
        end
        ENV["OLDPWD"] = new_oldpwd
        println(out, pwd())
    else
        run(ignorestatus(@static is_windows() ? cmd : (isa(STDIN, TTY) ? `$shell -i -c "$(shell_wrap_true(shell_name, cmd))"` : `$shell -c "$(shell_wrap_true(shell_name, cmd))"`)))
    end
    nothing
end

function shell_wrap_true(shell_name, cmd)
    if shell_name == "fish"
        "begin; $(shell_escape(cmd)); and true; end"
    else
        "($(shell_escape(cmd))) && true"
    end
end

function display_error(io::IO, er, bt)
    if !isempty(bt)
        st = stacktrace(bt)
        if !isempty(st)
            io = redirect(io, log_error_to, st[1])
        end
    end
    print_with_color(Base.error_color(), io, "ERROR: "; bold = true)
    # remove REPL-related frames from interactive printing
    eval_ind = findlast(addr->Base.REPL.ip_matches_func(addr, :eval), bt)
    if eval_ind != 0
        bt = bt[1:eval_ind-1]
    end
    showerror(IOContext(io, :limit => true), er, bt)
    println(io)
end
display_error(er, bt) = display_error(STDERR, er, bt)
display_error(er) = display_error(er, [])

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
                value = eval(Main, ast)
                eval(Main, Expr(:body, Expr(:(=), :ans, QuoteNode(value)), Expr(:return, nothing)))
                if !(value === nothing) && show_value
                    if have_color
                        print(answer_color())
                    end
                    try
                        eval(Main, Expr(:body, Expr(:return, Expr(:call, display, QuoteNode(value)))))
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

function parse_input_line(s::String; filename::String="none")
    # (expr, pos) = parse(s, 1)
    # (ex, pos) = ccall(:jl_parse_string, Any,
    #                   (Ptr{UInt8},Csize_t,Int32,Int32),
    #                   s, sizeof(s), pos-1, 1)
    # if ex!==()
    #     throw(ParseError("extra input after end of expression"))
    # end
    # expr
    ccall(:jl_parse_input_line, Any, (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t),
        s, sizeof(s), filename, sizeof(filename))
end
parse_input_line(s::AbstractString) = parse_input_line(String(s))

function parse_input_line(io::IO)
    s = ""
    while !eof(io)
        s *= readline(io, chomp=false)
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
        length(idxs) > 0 && deleteat!(ARGS, idxs[1])
    end
    repl                  = true
    startup               = (opts.startupfile != 2)
    history_file          = (opts.historyfile != 0)
    quiet                 = (opts.quiet != 0)
    color_set             = (opts.color != 0)
    global have_color     = (opts.color == 1)
    global is_interactive = (opts.isinteractive != 0)
    while true
        # startup worker.
        # opts.startupfile, opts.load, etc should should not be processed for workers.
        if opts.worker != C_NULL
            start_worker(unsafe_string(opts.worker)) # does not return
        end

        # add processors
        if opts.nprocs > 0
            addprocs(opts.nprocs)
        end
        # load processes from machine file
        if opts.machinefile != C_NULL
            addprocs(load_machine_file(unsafe_string(opts.machinefile)))
        end

        # load ~/.juliarc file
        startup && load_juliarc()

        # load file immediately on all processors
        if opts.load != C_NULL
            @sync for p in procs()
                @async remotecall_fetch(include, p, unsafe_string(opts.load))
            end
        end
        # eval expression
        if opts.eval != C_NULL
            repl = false
            eval(Main, parse_input_line(unsafe_string(opts.eval)))
            break
        end
        # eval expression and show result
        if opts.print != C_NULL
            repl = false
            show(eval(Main, parse_input_line(unsafe_string(opts.print))))
            println()
            break
        end
        # load file
        if !isempty(ARGS) && !isempty(ARGS[1])
            # program
            repl = false
            # remove filename from ARGS
            global PROGRAM_FILE = shift!(ARGS)
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
        s = split(line, '*'; keep = false)
        map!(strip, s, s)
        if length(s) > 1
            cnt = isnumber(s[1]) ? parse(Int,s[1]) : Symbol(s[1])
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

"""
    atreplinit(f)

Register a one-argument function to be called before the REPL interface is initialized in
interactive sessions; this is useful to customize the interface. The argument of `f` is the
REPL object. This function should be called from within the `.juliarc.jl` initialization
file.
"""
atreplinit(f::Function) = (unshift!(repl_hooks, f); nothing)

function __atreplinit(repl)
    for f in repl_hooks
        try
            f(repl)
        catch err
            showerror(STDERR, err)
            println(STDERR)
        end
    end
end
_atreplinit(repl) = @eval Main $__atreplinit($repl)

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
                global is_interactive |= !isa(STDIN, Union{File, IOStream})
                color_set || (global have_color = false)
            else
                term = Terminals.TTYTerminal(get(ENV, "TERM", @static is_windows() ? "" : "dumb"), STDIN, STDOUT, STDERR)
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
