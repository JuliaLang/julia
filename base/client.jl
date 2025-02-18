# This file is a part of Julia. License is MIT: https://julialang.org/license

## client.jl - frontend handling command line options, environment setup,
##             and REPL

have_color = nothing
have_truecolor = nothing
const default_color_warn = :yellow
const default_color_error = :light_red
const default_color_info = :cyan
const default_color_debug = :blue
const default_color_input = :normal
const default_color_answer = :normal
const color_normal = text_colors[:normal]

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
        else
            dir = homedir()
        end
        try
            ENV["OLDPWD"] = pwd()
        catch ex
            ex isa IOError || rethrow()
            # if current dir has been deleted, then pwd() will throw an IOError: pwd(): no such file or directory (ENOENT)
            delete!(ENV, "OLDPWD")
        end
        cd(dir)
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
        try
            run(ignorestatus(cmd))
        catch
            # Windows doesn't shell out right now (complex issue), so Julia tries to run the program itself
            # Julia throws an exception if it can't find the program, but the stack trace isn't useful
            lasterr = current_exceptions()
            lasterr = ExceptionStack([(exception = e[1], backtrace = [] ) for e in lasterr])
            invokelatest(display_error, lasterr)
        end
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
        bt = bt isa Vector{StackFrame} ? copy(bt) : stacktrace(bt)
        # remove REPL-related frames from interactive printing
        eval_ind = findlast(frame -> !frame.from_c && startswith(String(frame.func), "__repl_entry"), bt)
        eval_ind === nothing || deleteat!(bt, eval_ind:length(bt))
    end
    return bt
end
scrub_repl_backtrace(stack::ExceptionStack) =
    ExceptionStack(Any[(;x.exception, backtrace = scrub_repl_backtrace(x.backtrace)) for x in stack])

istrivialerror(stack::ExceptionStack) =
    length(stack) == 1 && length(stack[1].backtrace) ≤ 1 && !isa(stack[1].exception, MethodError)
    # frame 1 = top level; assumes already went through scrub_repl_backtrace; MethodError see #50803

function display_error(io::IO, stack::ExceptionStack)
    printstyled(io, "ERROR: "; bold=true, color=Base.error_color())
    show_exception_stack(IOContext(io, :limit => true), stack)
    println(io)
end
display_error(stack::ExceptionStack) = display_error(stderr, stack)

# these forms are depended on by packages outside Julia
function display_error(io::IO, er, bt)
    printstyled(io, "ERROR: "; bold=true, color=Base.error_color())
    showerror(IOContext(io, :limit => true), er, bt, backtrace = bt!==nothing)
    println(io)
end
display_error(er, bt=nothing) = display_error(stderr, er, bt)

function eval_user_input(errio, @nospecialize(ast), show_value::Bool)
    errcount = 0
    lasterr = nothing
    have_color = get(stdout, :color, false)::Bool
    while true
        try
            if have_color
                print(color_normal)
            end
            if lasterr !== nothing
                lasterr = scrub_repl_backtrace(lasterr)
                istrivialerror(lasterr) || setglobal!(Base.MainInclude, :err, lasterr)
                invokelatest(display_error, errio, lasterr)
                errcount = 0
                lasterr = nothing
            else
                ast = Meta.lower(Main, ast)
                value = Core.eval(Main, ast)
                setglobal!(Base.MainInclude, :ans, value)
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
                end
            end
            break
        catch
            if errcount > 0
                @error "SYSTEM: display_error(errio, lasterr) caused an error"
            end
            errcount += 1
            lasterr = scrub_repl_backtrace(current_exceptions())
            setglobal!(Base.MainInclude, :err, lasterr)
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
    ex = Meta.parseall(s, filename=filename)
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

# detect the reason which caused an :incomplete expression
# from the error message
# NOTE: the error messages are defined in src/julia-parser.scm
function fl_incomplete_tag(msg::AbstractString)
    occursin("string", msg) && return :string
    occursin("comment", msg) && return :comment
    occursin("requires end", msg) && return :block
    occursin("\"`\"", msg) && return :cmd
    occursin("character", msg) && return :char
    return :other
end

incomplete_tag(ex) = :none
function incomplete_tag(ex::Expr)
    if ex.head !== :incomplete
        return :none
    elseif isempty(ex.args)
        return :other
    elseif ex.args[1] isa String
        return fl_incomplete_tag(ex.args[1])
    else
        return incomplete_tag(ex.args[1])
    end
end
incomplete_tag(exc::Meta.ParseError) = incomplete_tag(exc.detail)

function exec_options(opts)
    startup               = (opts.startupfile != 2)
    global have_color     = colored_text(opts)
    global is_interactive = (opts.isinteractive != 0)

    # pre-process command line argument list
    arg_is_program = !isempty(ARGS)
    repl = !arg_is_program
    cmds = unsafe_load_commands(opts.commands)
    for (cmd, arg) in cmds
        if cmd_suppresses_program(cmd)
            arg_is_program = false
            repl = false
        elseif cmd == 'L' || cmd == 'm'
            # nothing
        elseif cmd == 'B' # --bug-report
            # If we're doing a bug report, don't load anything else. We will
            # spawn a child in which to execute these options.
            let InteractiveUtils = load_InteractiveUtils()
                invokelatest(InteractiveUtils.report_bug, arg)
            end
            return false
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
            MainInclude.Distributed = Distributed
            Core.eval(Main, :(using Base.MainInclude.Distributed))
            invokelatest(Distributed.process_opts, opts)
        end
    end

    interactiveinput = (repl || is_interactive::Bool) && isa(stdin, TTY)
    is_interactive::Bool |= interactiveinput

    # load terminfo in for styled printing
    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    global current_terminfo = load_terminfo(term_env)

    # load ~/.julia/config/startup.jl file
    if startup
        try
            load_julia_startup()
        catch
            invokelatest(display_error, scrub_repl_backtrace(current_exceptions()))
            !(repl || is_interactive::Bool) && exit(1)
        end
    end

    # process cmds list
    for (cmd, arg) in cmds
        if cmd == 'e'
            Core.eval(Main, parse_input_line(arg))
        elseif cmd == 'E'
            invokelatest(show, Core.eval(Main, parse_input_line(arg)))
            println()
        elseif cmd == 'm'
            entrypoint = push!(split(arg, "."), "main")
            Base.eval(Main, Expr(:import, Expr(:., Symbol.(entrypoint)...)))
            if !invokelatest(should_use_main_entrypoint)
                error("`main` in `$arg` not declared as entry point (use `@main` to do so)")
            end
            return false
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
        if !is_interactive::Bool
            exit_on_sigint(true)
        end
        try
            if PROGRAM_FILE == "-"
                include_string(Main, read(stdin, String), "stdin")
            else
                include(Main, PROGRAM_FILE)
            end
        catch
            invokelatest(display_error, scrub_repl_backtrace(current_exceptions()))
            if !is_interactive::Bool
                exit(1)
            end
        end
    end

    return repl
end

function _global_julia_startup_file()
    # If the user built us with a specific Base.SYSCONFDIR, check that location first for a startup.jl file
    # If it is not found, then continue on to the relative path based on Sys.BINDIR
    BINDIR = Sys.BINDIR
    SYSCONFDIR = Base.SYSCONFDIR
    p1 = nothing
    if !isempty(SYSCONFDIR)
        p1 = abspath(BINDIR, SYSCONFDIR, "julia", "startup.jl")
        isfile(p1) && return p1
    end
    p2 = abspath(BINDIR, "..", "etc", "julia", "startup.jl")
    p1 == p2 && return nothing # don't check the same path twice
    isfile(p2) && return p2
    return nothing
end

function _local_julia_startup_file()
    if !isempty(DEPOT_PATH)
        path = abspath(DEPOT_PATH[1], "config", "startup.jl")
        isfile(path) && return path
    end
    return nothing
end

function load_julia_startup()
    global_file = _global_julia_startup_file()
    (global_file !== nothing) && include(Main, global_file)
    local_file = _local_julia_startup_file()
    (local_file !== nothing) && include(Main, local_file)
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

function load_InteractiveUtils(mod::Module=Main)
    # load interactive-only libraries
    if !isdefined(MainInclude, :InteractiveUtils)
        try
            # TODO: we have to use require_stdlib here because it is a dependency of REPL, but we would sort of prefer not to
            let InteractiveUtils = require_stdlib(PkgId(UUID(0xb77e0a4c_d291_57a0_90e8_8db25a27a240), "InteractiveUtils"))
                MainInclude.InteractiveUtils = InteractiveUtils
            end
        catch ex
            @warn "Failed to import InteractiveUtils into module $mod" exception=(ex, catch_backtrace())
            return nothing
        end
    end
    return Core.eval(mod, :(using Base.MainInclude.InteractiveUtils; Base.MainInclude.InteractiveUtils))
end

function load_REPL()
    # load interactive-only libraries
    try
        return Base.require_stdlib(PkgId(UUID(0x3fa0cd96_eef1_5676_8a61_b3b8758bbffb), "REPL"))
    catch ex
        @warn "Failed to import REPL" exception=(ex, catch_backtrace())
    end
    return nothing
end

global active_repl::Any
global active_repl_backend = nothing

function run_fallback_repl(interactive::Bool)
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
            while !eof(input)
                if interactive
                    print("julia> ")
                    flush(stdout)
                end
                try
                    line = ""
                    ex = nothing
                    while !eof(input)
                        line *= readline(input, keep=true)
                        ex = parse_input_line(line)
                        if !(isa(ex, Expr) && ex.head === :incomplete)
                            break
                        end
                    end
                    eval_user_input(stderr, ex, true)
                catch err
                    isa(err, InterruptException) ? print("\n\n") : rethrow()
                end
            end
        end
    end
    nothing
end

function run_std_repl(REPL::Module, quiet::Bool, banner::Symbol, history_file::Bool)
    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    term = REPL.Terminals.TTYTerminal(term_env, stdin, stdout, stderr)
    banner == :no || REPL.banner(term, short=banner==:short)
    if term.term_type == "dumb"
        repl = REPL.BasicREPL(term)
        quiet || @warn "Terminal not fully functional"
    else
        repl = REPL.LineEditREPL(term, get(stdout, :color, false), true)
        repl.history_file = history_file
    end
    # Make sure any displays pushed in .julia/config/startup.jl ends up above the
    # REPLDisplay
    d = REPL.REPLDisplay(repl)
    last_active_repl = @isdefined(active_repl) ? active_repl : nothing
    last_active_repl_backend = active_repl_backend
    global active_repl = repl
    pushdisplay(d)
    try
        global active_repl = repl
        _atreplinit(repl)
        REPL.run_repl(repl, backend->(global active_repl_backend = backend))
    finally
        popdisplay(d)
        active_repl = last_active_repl
        active_repl_backend = last_active_repl_backend
    end
    nothing
end

# run the requested sort of evaluation loop on stdio
function run_main_repl(interactive::Bool, quiet::Bool, banner::Symbol, history_file::Bool)
    fallback_repl = parse(Bool, get(ENV, "JULIA_FALLBACK_REPL", "false"))
    if !fallback_repl && interactive
        load_InteractiveUtils()
        REPL = REPL_MODULE_REF[]
        if REPL === Base
            load_REPL()
        end
    end
    REPL = REPL_MODULE_REF[]
    if !fallback_repl && interactive && REPL !== Base
        invokelatest(run_std_repl, REPL, quiet, banner, history_file)
    else
        if !fallback_repl && interactive && !quiet
            @warn "REPL provider not available: using basic fallback" LOAD_PATH=join(Base.LOAD_PATH, Sys.iswindows() ? ';' : ':')
        end
        run_fallback_repl(interactive)
    end
    nothing
end

# MainInclude exists to weakly add certain identifiers to Main
baremodule MainInclude
using ..Base

"""
    ans

A variable referring to the last computed value, automatically imported to the interactive prompt.
"""
global ans = nothing

"""
    err

A variable referring to the last thrown errors, automatically imported to the interactive prompt.
The thrown errors are collected in a stack of exceptions.
"""
global err = nothing

# Used for memoizing require_stdlib of these modules
global InteractiveUtils::Module
global Distributed::Module

# weakly exposes ans and err variables to Main
export ans, err
end

function should_use_main_entrypoint()
    isdefined(Main, :main) || return false
    M_binding_owner = Base.binding_module(Main, :main)
    (isdefined(M_binding_owner, Symbol("#__main_is_entrypoint__#")) && M_binding_owner.var"#__main_is_entrypoint__#") || return false
    return true
end

function _start()
    empty!(ARGS)
    append!(ARGS, Core.ARGS)
    # clear any postoutput hooks that were saved in the sysimage
    empty!(Base.postoutput_hooks)
    local ret = 0
    try
        repl_was_requested = exec_options(JLOptions())
        if invokelatest(should_use_main_entrypoint) && !is_interactive
            main = invokelatest(getglobal, Main, :main)
            if Base.generating_output()
                precompile(main, (typeof(ARGS),))
            else
                ret = invokelatest(main, ARGS)
            end
        elseif (repl_was_requested || is_interactive)
            # Run the Base `main`, which will either load the REPL stdlib
            # or run the fallback REPL
            ret = repl_main(ARGS)
        end
        ret === nothing && (ret = 0)
        ret = Cint(ret)
    catch
        ret = Cint(1)
        invokelatest(display_error, scrub_repl_backtrace(current_exceptions()))
    end
    if is_interactive && get(stdout, :color, false)
        print(color_normal)
    end
    return ret
end

function repl_main(_)
    opts = Base.JLOptions()
    interactiveinput = isa(stdin, Base.TTY)
    b = opts.banner
    auto = b == -1
    banner = b == 0 || (auto && !interactiveinput) ? :no  :
             b == 1 || (auto && interactiveinput)  ? :yes :
             :short # b == 2

    quiet                 = (opts.quiet != 0)
    history_file          = (opts.historyfile != 0)
    return run_main_repl(interactiveinput, quiet, banner, history_file)
end

"""
    @main

This macro is used to mark that the binding `main` in the current module is considered an
entrypoint. The precise semantics of the entrypoint depend on the CLI driver.

In the `julia` driver, if `Main.main` is marked as an entrypoint, it will be automatically called upon
the completion of script execution.

The `@main` macro may be used standalone or as part of the function definition, though in the latter
case, parentheses are required. In particular, the following are equivalent:

```
function @main(args)
    println("Hello World")
end
```

```
function main(args)
end
@main
```

## Detailed semantics

The entrypoint semantics attach to the owner of the binding owner. In particular, if a marked entrypoint is
imported into `Main`, it will be treated as an entrypoint in `Main`:

```
module MyApp
    export main
    @main(args) = println("Hello World")
end
using .MyApp
# `julia` Will execute MyApp.main at the conclusion of script execution
```

Note that in particular, the semantics do not attach to the method
or the name:
```
module MyApp
    @main(args) = println("Hello World")
end
const main = MyApp.main
# `julia` Will *NOT* execute MyApp.main unless there is a separate `@main` annotation in `Main`
```

!!! compat "Julia 1.11"
    This macro is new in Julia 1.11. At present, the precise semantics of `@main` are still subject to change.
"""
macro main(args...)
    if isdefined(__module__, :main)
        if Base.binding_module(__module__, :main) !== __module__
            error("Symbol `main` is already a resolved import in module $(__module__). `@main` must be used in the defining module.")
        end
    end
    Core.eval(__module__, quote
        # Force the binding to resolve to this module
        global main
        global var"#__main_is_entrypoint__#"::Bool = true
    end)
    if !isempty(args)
        Expr(:call, esc(:main), map(esc, args)...)
    else
        esc(:main)
    end
end
