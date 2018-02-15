# This file is a part of Julia. License is MIT: https://julialang.org/license

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
    :underline     => "\033[4m",
    :blink         => "\033[5m",
    :reverse       => "\033[7m",
    :hidden        => "\033[8m",
    :nothing       => "",
)

for i in 0:255
    text_colors[i] = "\033[38;5;$(i)m"
end

const disable_text_style = AnyDict(
    :bold      => "\033[22m",
    :underline => "\033[24m",
    :blink     => "\033[25m",
    :reverse   => "\033[27m",
    :hidden    => "\033[28m",
    :normal    => "",
    :default   => "",
    :nothing   => "",
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
default_color_debug = :blue
default_color_input = :normal
default_color_answer = :normal
color_normal = text_colors[:normal]

function repl_color(key, default)
    env_str = get(ENV, key, "")
    c = tryparse(Int, env_str)
    c_conv = coalesce(c, Symbol(env_str))
    haskey(text_colors, c_conv) ? c_conv : default
end

error_color() = repl_color("JULIA_ERROR_COLOR", default_color_error)
warn_color()  = repl_color("JULIA_WARN_COLOR" , default_color_warn)
info_color()  = repl_color("JULIA_INFO_COLOR" , default_color_info)
debug_color()  = repl_color("JULIA_DEBUG_COLOR" , default_color_debug)

input_color()  = text_colors[repl_color("JULIA_INPUT_COLOR", default_color_input)]
answer_color() = text_colors[repl_color("JULIA_ANSWER_COLOR", default_color_answer)]

stackframe_lineinfo_color() = repl_color("JULIA_STACKFRAME_LINEINFO_COLOR", :bold)
stackframe_function_color() = repl_color("JULIA_STACKFRAME_FUNCTION_COLOR", :bold)

function repl_cmd(cmd, out)
    shell = shell_split(get(ENV, "JULIA_SHELL", get(ENV, "SHELL", "/bin/sh")))
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
                @static if !Sys.iswindows()
                    # TODO: this is a rather expensive way to copy a string, remove?
                    # If it's intended to simulate `cd`, it should instead be doing
                    # more nearly `cd $dir && printf %s \$PWD` (with appropriate quoting),
                    # since shell `cd` does more than just `echo` the result.
                    dir = read(`$shell -c "printf %s $(shell_escape_posixly(dir))"`, String)
                end
                cd(dir)
            end
        else
            cd()
        end
        ENV["OLDPWD"] = new_oldpwd
        println(out, pwd())
    else
        @static if !Sys.iswindows()
            if shell_name == "fish"
                shell_escape_cmd = "begin; $(shell_escape_posixly(cmd)); and true; end"
            else
                shell_escape_cmd = "($(shell_escape_posixly(cmd))) && true"
            end
            cmd = `$shell -c $shell_escape_cmd`
        end
        run(ignorestatus(cmd))
    end
    nothing
end

function ip_matches_func(ip, func::Symbol)
    for fr in StackTraces.lookup(ip)
        if fr === StackTraces.UNKNOWN || fr.from_c
            return false
        end
        fr.func === func && return true
    end
    return false
end

function display_error(io::IO, er, bt)
    if !isempty(bt)
        st = stacktrace(bt)
        if !isempty(st)
            io = redirect(io, log_error_to, st[1])
        end
    end
    printstyled(io, "ERROR: "; bold=true, color=Base.error_color())
    # remove REPL-related frames from interactive printing
    eval_ind = findlast(addr->ip_matches_func(addr, :eval), bt)
    if eval_ind !== nothing
        bt = bt[1:eval_ind-1]
    end
    showerror(IOContext(io, :limit => true), er, bt)
    println(io)
end
display_error(er, bt) = display_error(stderr, er, bt)
display_error(er) = display_error(er, [])

function eval_user_input(@nospecialize(ast), show_value::Bool)
    errcount, lasterr, bt = 0, (), nothing
    while true
        try
            if have_color
                print(color_normal)
            end
            if errcount > 0
                invokelatest(display_error, lasterr, bt)
                errcount, lasterr = 0, ()
            else
                ast = Meta.lower(Main, ast)
                value = eval(Main, ast)
                eval(Main, Expr(:body, Expr(:(=), :ans, QuoteNode(value)), Expr(:return, nothing)))
                if !(value === nothing) && show_value
                    if have_color
                        print(answer_color())
                    end
                    try
                        invokelatest(display, value)
                    catch err
                        println(stderr, "Evaluation succeeded, but an error occurred while showing value of type ", typeof(value), ":")
                        rethrow(err)
                    end
                    println()
                end
            end
            break
        catch err
            if errcount > 0
                println(stderr, "SYSTEM: show(lasterr) caused an error")
            end
            errcount, lasterr = errcount+1, err
            if errcount > 2
                println(stderr, "WARNING: it is likely that something important is broken, and Julia will not be able to continue normally")
                break
            end
            bt = catch_backtrace()
        end
    end
    isa(stdin, TTY) && println()
    nothing
end

function parse_input_line(s::String; filename::String="none", depwarn=true)
    # For now, assume all parser warnings are depwarns
    ex = with_logger(depwarn ? current_logger() : NullLogger()) do
        ccall(:jl_parse_input_line, Any, (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t),
              s, sizeof(s), filename, sizeof(filename))
    end
    if ex isa Symbol && all(equalto('_'), string(ex))
        # remove with 0.7 deprecation
        Meta.lower(Main, ex)  # to get possible warning about using _ as an rvalue
    end
    return ex
end
parse_input_line(s::AbstractString) = parse_input_line(String(s))

function parse_input_line(io::IO)
    s = ""
    while !eof(io)
        s *= readline(io, keep=true)
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

# call include() on a file, ignoring if not found
include_ifexists(mod::Module, path::AbstractString) = isfile(path) && include(mod, path)

function exec_options(opts)
    if !isempty(ARGS)
        idxs = findall(x -> x == "--", ARGS)
        length(idxs) > 0 && deleteat!(ARGS, idxs[1])
    end
    quiet                 = (opts.quiet != 0)
    startup               = (opts.startupfile != 2)
    history_file          = (opts.historyfile != 0)
    color_set             = (opts.color != 0) # --color!=auto
    global have_color     = (opts.color == 1) # --color=on
    global is_interactive = (opts.isinteractive != 0)

    # pre-process command line argument list
    arg_is_program = !isempty(ARGS)
    repl = !arg_is_program
    cmds = unsafe_load_commands(opts.commands)
    for (cmd, arg) in cmds
        if cmd == 'e'
            arg_is_program = false
            repl = false
        elseif cmd == 'E'
            arg_is_program = false
            repl = false
        elseif cmd == 'L'
            # nothing
        else
            @warn "Unexpected command -$cmd'$arg'"
        end
    end

    # remove filename from ARGS
    global PROGRAM_FILE = arg_is_program ? popfirst!(ARGS) : ""

    # Load Distributed module only if any of the Distributed options have been specified.
    distributed_mode = (opts.worker == 1) || (opts.nprocs > 0) || (opts.machine_file != C_NULL)
    if distributed_mode
        eval(Main, :(using Distributed))
        invokelatest(Main.Distributed.process_opts, opts)
    end

    # load ~/.juliarc file
    startup && load_juliarc()

    if repl || is_interactive
        # load interactive-only libraries
        eval(Main, :(using InteractiveUtils))
    end

    # process cmds list
    for (cmd, arg) in cmds
        if cmd == 'e'
            eval(Main, parse_input_line(arg))
        elseif cmd == 'E'
            invokelatest(show, eval(Main, parse_input_line(arg)))
            println()
        elseif cmd == 'L'
            # load file immediately on all processors
            if !distributed_mode
                include(Main, arg)
            else
                # TODO: Move this logic to Distributed and use a callback
                @sync for p in invokelatest(Main.procs)
                    @async invokelatest(Main.remotecall_wait, include, p, Main, arg)
                end
            end
        end
    end

    # load file
    if arg_is_program
        # program
        if !is_interactive
            ccall(:jl_exit_on_sigint, Cvoid, (Cint,), 1)
        end
        include(Main, PROGRAM_FILE)
    end
    repl |= is_interactive
    if repl
        interactiveinput = isa(stdin, TTY)
        if interactiveinput
            global is_interactive = true
            banner = (opts.banner != 0) # --banner!=no
        else
            banner = (opts.banner == 1) # --banner=yes
        end
        run_main_repl(interactiveinput, quiet, banner, history_file, color_set)
    end
    nothing
end

function load_juliarc()
    # If the user built us with a specific Base.SYSCONFDIR, check that location first for a juliarc.jl file
    #   If it is not found, then continue on to the relative path based on Sys.BINDIR
    if !isempty(Base.SYSCONFDIR) && isfile(joinpath(Sys.BINDIR, Base.SYSCONFDIR, "julia", "juliarc.jl"))
        include(Main, abspath(Sys.BINDIR, Base.SYSCONFDIR, "julia", "juliarc.jl"))
    else
        include_ifexists(Main, abspath(Sys.BINDIR, "..", "etc", "julia", "juliarc.jl"))
    end
    include_ifexists(Main, abspath(homedir(), ".juliarc.jl"))
    nothing
end

const repl_hooks = []

"""
    atreplinit(f)

Register a one-argument function to be called before the REPL interface is initialized in
interactive sessions; this is useful to customize the interface. The argument of `f` is the
REPL object. This function should be called from within the `.juliarc.jl` initialization
file.
"""
atreplinit(f::Function) = (pushfirst!(repl_hooks, f); nothing)

function __atreplinit(repl)
    for f in repl_hooks
        try
            f(repl)
        catch err
            showerror(stderr, err)
            println(stderr)
        end
    end
end
_atreplinit(repl) = invokelatest(__atreplinit, repl)

# The REPL stdlib hooks into Base using this Ref
const REPL_MODULE_REF = Ref{Module}()

# run the requested sort of evaluation loop on stdio
function run_main_repl(interactive::Bool, quiet::Bool, banner::Bool, history_file::Bool, color_set::Bool)
    global active_repl
    if interactive && isassigned(REPL_MODULE_REF)
        invokelatest(REPL_MODULE_REF[]) do REPL
            term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
            term = REPL.Terminals.TTYTerminal(term_env, stdin, stdout, stderr)
            color_set || (global have_color = REPL.Terminals.hascolor(term))
            banner && REPL.banner(term, term)
            if term.term_type == "dumb"
                active_repl = REPL.BasicREPL(term)
                quiet || @warn "Terminal not fully functional"
            else
                active_repl = REPL.LineEditREPL(term, have_color, true)
                active_repl.history_file = history_file
            end
            # Make sure any displays pushed in .juliarc.jl ends up above the
            # REPLDisplay
            pushdisplay(REPL.REPLDisplay(active_repl))
            _atreplinit(active_repl)
            REPL.run_repl(active_repl, backend->(global active_repl_backend = backend))
        end
    else
        # otherwise provide a simple fallback
        if interactive && !quiet
            @warn "REPL provider not available: using basic fallback"
        end
        banner && Base.banner()
        let input = stdin
            if isa(input, File) || isa(input, IOStream)
                # for files, we can slurp in the whole thing at once
                ex = parse_input_line(read(input, String))
                if Meta.isexpr(ex, :toplevel)
                    # if we get back a list of statements, eval them sequentially
                    # as if we had parsed them sequentially
                    for stmt in ex.args
                        eval_user_input(stmt, true)
                    end
                    body = ex.args
                else
                    eval_user_input(ex, true)
                end
            else
                while isopen(input) || !eof(input)
                    if interactive
                        print("julia> ")
                        flush(stdout)
                    end
                    eval_user_input(parse_input_line(input), true)
                end
            end
        end
    end
    nothing
end

function _start()
    empty!(ARGS)
    append!(ARGS, Core.ARGS)
    @eval Main using Base.MainInclude
    try
        exec_options(JLOptions())
    catch err
        invokelatest(display_error, err, catch_backtrace())
        exit(1)
    end
    if is_interactive && have_color
        print(color_normal)
    end
end
