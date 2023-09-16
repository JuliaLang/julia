# This file is a part of Julia. License is MIT: https://julialang.org/license

module Client

import Base: PkgId, UUID
using Base.Filesystem

# We are losing delay loading of these anyway,
# we depend on them during precompile and for consistencies sake
# Julia will automatically load them for us.

import Pkg
import REPL
import InteractiveUtils

md_suppresses_program(cmd) = cmd in ('e', 'E')
function exec_options(opts)
    quiet                 = (opts.quiet != 0)
    startup               = (opts.startupfile != 2)
    history_file          = (opts.historyfile != 0)
    color_set             = (opts.color != 0) # --color!=auto
    Base.have_color = color_set ? (opts.color == 1) : nothing # --color=on
    Base.is_interactive = (opts.isinteractive != 0)

    # pre-process command line argument list
    arg_is_program = !isempty(ARGS)
    repl = !arg_is_program
    cmds = Base.unsafe_load_commands(opts.commands)
    for (cmd, arg) in cmds
        if Base.cmd_suppresses_program(cmd)
            arg_is_program = false
            repl = false
        elseif cmd == 'L'
            # nothing
        elseif cmd == 'B' # --bug-report
            # If we're doing a bug report, don't load anything else. We will
            # spawn a child in which to execute these options.
            let InteractiveUtils = load_InteractiveUtils()
                InteractiveUtils.report_bug(arg)
            end
            return nothing
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

    interactiveinput = (repl || Base.is_interactive::Bool) && isa(stdin, Base.TTY)
    Base.is_interactive::Bool |= interactiveinput

    # load ~/.julia/config/startup.jl file
    if startup
        try
            load_julia_startup()
        catch
            invokelatest(Base.display_error, Base.scrub_repl_backtrace(current_exceptions()))
            !(repl || Base.is_interactive::Bool) && exit(1)
        end
    end

    # process cmds list
    for (cmd, arg) in cmds
        if cmd == 'e'
            Core.eval(Main, Base.parse_input_line(arg))
        elseif cmd == 'E'
            invokelatest(show, Core.eval(Main, Base.parse_input_line(arg)))
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
        if !Base.is_interactive::Bool
            Base.exit_on_sigint(true)
        end
        try
            if PROGRAM_FILE == "-"
                Base.include_string(Main, read(stdin, String), "stdin")
            else
                Base.include(Main, PROGRAM_FILE)
            end
        catch
            invokelatest(Base.display_error, Base.scrub_repl_backtrace(current_exceptions()))
            if !Base.is_interactive::Bool
                exit(1)
            end
        end
    end
    if repl || Base.is_interactive::Bool
        b = opts.banner
        auto = b == -1
        banner = b == 0 || (auto && !interactiveinput) ? :no  :
                 b == 1 || (auto && interactiveinput)  ? :yes :
                 :short # b == 2
        run_main_repl(interactiveinput, quiet, banner, history_file, color_set)
    end
    nothing
end

function _global_julia_startup_file()
    # If the user built us with a specific Base.SYSCONFDIR, check that location first for a startup.jl file
    # If it is not found, then continue on to the relative path based on Sys.BINDIR
    BINDIR = Sys.BINDIR
    SYSCONFDIR = Base.SYSCONFDIR
    if !isempty(SYSCONFDIR)
        p1 = abspath(BINDIR, SYSCONFDIR, "julia", "startup.jl")
        isfile(p1) && return p1
    end
    p2 = abspath(BINDIR, "..", "etc", "julia", "startup.jl")
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
    (global_file !== nothing) && Base.include(Main, global_file)
    local_file = _local_julia_startup_file()
    (local_file !== nothing) && Base.include(Main, local_file)
    return nothing
end

function load_InteractiveUtils(mod::Module=Main)
    # load interactive-only libraries
    if !isdefined(mod, :InteractiveUtils)
        try
            let InteractiveUtils = Base.require(PkgId(UUID(0xb77e0a4c_d291_57a0_90e8_8db25a27a240), "InteractiveUtils"))
                Core.eval(mod, :(const InteractiveUtils = $InteractiveUtils))
                Core.eval(mod, :(using .InteractiveUtils))
                return InteractiveUtils
            end
        catch ex
            @warn "Failed to import InteractiveUtils into module $mod" exception=(ex, catch_backtrace())
        end
        return nothing
    end
    return getfield(mod, :InteractiveUtils)
end

# run the requested sort of evaluation loop on stdio
function run_main_repl(interactive::Bool, quiet::Bool, banner::Symbol, history_file::Bool, color_set::Bool)
    load_InteractiveUtils()

    if interactive && isassigned(Base.REPL_MODULE_REF)
        invokelatest(Base.REPL_MODULE_REF[]) do REPL
            term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
            Base.current_terminfo = Base.load_terminfo(term_env)
            term = REPL.Terminals.TTYTerminal(term_env, stdin, stdout, stderr)
            banner == :no || Base.banner(term, short=banner==:short)
            if term.term_type == "dumb"
                repl = REPL.BasicREPL(term)
                quiet || @warn "Terminal not fully functional"
            else
                repl = REPL.LineEditREPL(term, get(stdout, :color, false), true)
                repl.history_file = history_file
            end
            Base.active_repl = repl
            # Make sure any displays pushed in .julia/config/startup.jl ends up above the
            # REPLDisplay
            pushdisplay(REPL.REPLDisplay(repl))
            Base._atreplinit(repl)
            REPL.run_repl(repl, backend->(Base.active_repl_backend = backend))
        end
    else
        # otherwise provide a simple fallback
        if interactive && !quiet
            @warn "REPL provider not available: using basic fallback"
        end
        banner == :no || Base.banner(short=banner==:short)
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
    end
    nothing
end

if Base.generating_output()
    include("precompile.jl")
end

end # module Driver
