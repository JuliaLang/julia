# This file is a part of Julia. License is MIT: https://julialang.org/license

## client.jl - frontend handling command line options, environment setup,
##             and REPL

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
    c_conv = something(c, Symbol(env_str))
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

    # Immediately expand all arguments, so that typing e.g. ~/bin/foo works.
    cmd.exec .= expanduser.(cmd.exec)

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
                dir = ENV["OLDPWD"]
            end
            cd(dir)
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

# deprecated function--preserved for DocTests.jl
function ip_matches_func(ip, func::Symbol)
    for fr in StackTraces.lookup(ip)
        if fr === StackTraces.UNKNOWN || fr.from_c
            return false
        end
        fr.func === func && return true
    end
    return false
end

function scrub_repl_backtrace(bt)
    if bt !== nothing && !(bt isa Vector{Any}) # ignore our sentinel value types
        bt = stacktrace(bt)
        # remove REPL-related frames from interactive printing
        eval_ind = findlast(frame -> !frame.from_c && frame.func === :eval, bt)
        eval_ind === nothing || deleteat!(bt, eval_ind:length(bt))
    end
    return bt
end

function display_error(io::IO, er, bt)
    printstyled(io, "ERROR: "; bold=true, color=Base.error_color())
    bt = scrub_repl_backtrace(bt)
    showerror(IOContext(io, :limit => true), er, bt, backtrace = bt!==nothing)
    println(io)
end
function display_error(io::IO, stack::Vector)
    printstyled(io, "ERROR: "; bold=true, color=Base.error_color())
    bt = Any[ (x[1], scrub_repl_backtrace(x[2])) for x in stack ]
    show_exception_stack(IOContext(io, :limit => true), bt)
end
display_error(stack::Vector) = display_error(stderr, stack)
display_error(er, bt=nothing) = display_error(stderr, er, bt)

function eval_user_input(errio, @nospecialize(ast), show_value::Bool)
    errcount = 0
    lasterr = nothing
    while true
        try
            if have_color
                print(color_normal)
            end
            if lasterr !== nothing
                invokelatest(display_error, errio, lasterr)
                errcount = 0
                lasterr = nothing
            else
                ast = Meta.lower(Main, ast)
                value = Core.eval(Main, ast)
                ccall(:jl_set_global, Cvoid, (Any, Any, Any), Main, :ans, value)
                if !(value === nothing) && show_value
                    if have_color
                        print(answer_color())
                    end
                    try
                        invokelatest(display, value)
                    catch
                        @error "Evaluation succeeded, but an error occurred while displaying the value" typeof(value)
                        rethrow()
                    end
                    println()
                end
            end
            break
        catch
            if errcount > 0
                @error "SYSTEM: display_error(errio, lasterr) caused an error"
            end
            errcount += 1
            lasterr = catch_stack()
            if errcount > 2
                @error "It is likely that something important is broken, and Julia will not be able to continue normally" errcount
                break
            end
        end
    end
    isa(stdin, TTY) && println()
    nothing
end

function _parse_input_line_core(s::String, filename::String)
    ex = ccall(:jl_parse_all, Any, (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t),
               s, sizeof(s), filename, sizeof(filename))
    if ex isa Expr && ex.head === :toplevel
        if isempty(ex.args)
            return nothing
        end
        last = ex.args[end]
        if last isa Expr && (last.head === :error || last.head === :incomplete)
            # if a parse error happens in the middle of a multi-line input
            # return only the error, so that none of the input is evaluated.
            return last
        end
    end
    return ex
end

function parse_input_line(s::String; filename::String="none", depwarn=true)
    # For now, assume all parser warnings are depwarns
    ex = if depwarn
        _parse_input_line_core(s, filename)
    else
        with_logger(NullLogger()) do
            _parse_input_line_core(s, filename)
        end
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
    occursin("string", msg) && return :string
    occursin("comment", msg) && return :comment
    occursin("requires end", msg) && return :block
    occursin("\"`\"", msg) && return :cmd
    occursin("character", msg) && return :char
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
        let Distributed = require(PkgId(UUID((0x8ba89e20_285c_5b6f, 0x9357_94700520ee1b)), "Distributed"))
            Core.eval(Main, :(const Distributed = $Distributed))
            Core.eval(Main, :(using .Distributed))
        end

        invokelatest(Main.Distributed.process_opts, opts)
    end

    # load ~/.julia/config/startup.jl file
    startup && load_julia_startup()

    # process cmds list
    for (cmd, arg) in cmds
        if cmd == 'e'
            Core.eval(Main, parse_input_line(arg))
        elseif cmd == 'E'
            invokelatest(show, Core.eval(Main, parse_input_line(arg)))
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
        try
            include(Main, PROGRAM_FILE)
        catch
            invokelatest(display_error, catch_stack())
            if !is_interactive
                exit(1)
            end
        end
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

function load_julia_startup()
    # If the user built us with a specific Base.SYSCONFDIR, check that location first for a startup.jl file
    #   If it is not found, then continue on to the relative path based on Sys.BINDIR
    BINDIR = Sys.BINDIR::String
    SYSCONFDIR = Base.SYSCONFDIR::String
    if !isempty(SYSCONFDIR) && isfile(joinpath(BINDIR, SYSCONFDIR, "julia", "startup.jl"))
        include(Main, abspath(BINDIR, SYSCONFDIR, "julia", "startup.jl"))
    else
        include_ifexists(Main, abspath(BINDIR, "..", "etc", "julia", "startup.jl"))
    end
    !isempty(DEPOT_PATH) && include_ifexists(Main, abspath(DEPOT_PATH[1], "config", "startup.jl"))
    return nothing
end

const repl_hooks = []

"""
    atreplinit(f)

Register a one-argument function to be called before the REPL interface is initialized in
interactive sessions; this is useful to customize the interface. The argument of `f` is the
REPL object. This function should be called from within the `.julia/config/startup.jl`
initialization file.
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
    # load interactive-only libraries
    if !isdefined(Main, :InteractiveUtils)
        try
            let InteractiveUtils = require(PkgId(UUID(0xb77e0a4c_d291_57a0_90e8_8db25a27a240), "InteractiveUtils"))
                Core.eval(Main, :(const InteractiveUtils = $InteractiveUtils))
                Core.eval(Main, :(using .InteractiveUtils))
            end
        catch ex
            @warn "Failed to import InteractiveUtils into module Main" exception=(ex, catch_backtrace())
        end
    end

    if interactive && isassigned(REPL_MODULE_REF)
        invokelatest(REPL_MODULE_REF[]) do REPL
            term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
            term = REPL.Terminals.TTYTerminal(term_env, stdin, stdout, stderr)
            color_set || (global have_color = REPL.Terminals.hascolor(term))
            banner && Base.banner(term)
            if term.term_type == "dumb"
                active_repl = REPL.BasicREPL(term)
                quiet || @warn "Terminal not fully functional"
            else
                active_repl = REPL.LineEditREPL(term, have_color, true)
                active_repl.history_file = history_file
            end
            # Make sure any displays pushed in .julia/config/startup.jl ends up above the
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
                        eval_user_input(stderr, stmt, true)
                    end
                    body = ex.args
                else
                    eval_user_input(stderr, ex, true)
                end
            else
                while isopen(input) || !eof(input)
                    if interactive
                        print("julia> ")
                        flush(stdout)
                    end
                    try
                        eval_user_input(stderr, parse_input_line(input), true)
                    catch err
                        isa(err, InterruptException) ? print("\n\n") : rethrow()
                    end
                end
            end
        end
    end
    nothing
end

# MainInclude exists to hide Main.include and eval from `names(Main)`.
baremodule MainInclude
using ..Base
# We inline the definition of include from loading.jl/include_relative to get one-frame stacktraces.
# include(fname::AbstractString) = Main.Base.include(Main, fname)
include(fname::AbstractString) = include(identity, fname)
function include(mapexpr::Function, fname::AbstractString)
    mod = Main
    isa(fname, String) || (fname = Base.convert(String, fname)::String)
    path, prev = Base._include_dependency(mod, fname)
    for callback in Base.include_callbacks # to preserve order, must come before Core.include
        Base.invokelatest(callback, mod, path)
    end
    tls = Base.task_local_storage()
    tls[:SOURCE_PATH] = path
    local result
    try
        if mapexpr === identity
            result = ccall(:jl_load, Any, (Any, Cstring), mod, path)
        else
            result = ccall(:jl_load_rewrite, Any, (Any, Cstring, Any), mod, path, mapexpr)
        end
    finally
        if prev === nothing
            Base.delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
    return result
end
eval(x) = Core.eval(Main, x)
end

"""
    eval(expr)

Evaluate an expression in the global scope of the containing module.
Every `Module` (except those defined with `baremodule`) has its own 1-argument
definition of `eval`, which evaluates expressions in that module.
"""
MainInclude.eval

"""
    include(path::AbstractString)

Evaluate the contents of the input source file in the global scope of the containing module.
Every module (except those defined with `baremodule`) has its own 1-argument
definition of `include`, which evaluates the file in that module.
Returns the result of the last evaluated expression of the input file. During including,
a task-local include path is set to the directory containing the file. Nested calls to
`include` will search relative to that path. This function is typically used to load source
interactively, or to combine files in packages that are broken into multiple source files.

Use [`Base.include`](@ref) to evaluate a file into another module.
"""
MainInclude.include

function _start()
    empty!(ARGS)
    append!(ARGS, Core.ARGS)
    if ccall(:jl_generating_output, Cint, ()) != 0 && JLOptions().incremental == 0
        # clear old invalid pointers
        PCRE.__init__()
    end
    try
        exec_options(JLOptions())
    catch
        invokelatest(display_error, catch_stack())
        exit(1)
    end
    if is_interactive && have_color
        print(color_normal)
    end
end
