# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Run Evaluate Print Loop (REPL)

    Example minimal code
    ```
    import REPL
    term = REPL.Terminals.TTYTerminal("dumb", stdin, stdout, stderr)
    repl = REPL.LineEditREPL(term, true)
    REPL.run_repl(repl)
    ```
"""
module REPL

Base.Experimental.@optlevel 1

using Base.Meta, Sockets
import InteractiveUtils

export
    AbstractREPL,
    BasicREPL,
    LineEditREPL,
    StreamREPL

import Base:
    AbstractDisplay,
    display,
    show,
    AnyDict,
    ==

_displaysize(io::IO) = displaysize(io)::Tuple{Int,Int}

include("Terminals.jl")
using .Terminals

abstract type AbstractREPL end

include("options.jl")

include("LineEdit.jl")
using .LineEdit
import ..LineEdit:
    CompletionProvider,
    HistoryProvider,
    add_history,
    complete_line,
    history_next,
    history_next_prefix,
    history_prev,
    history_prev_prefix,
    history_first,
    history_last,
    history_search,
    accept_result,
    setmodifiers!,
    terminal,
    MIState,
    PromptState,
    TextInterface

include("REPLCompletions.jl")
using .REPLCompletions

include("TerminalMenus/TerminalMenus.jl")
include("docview.jl")

@nospecialize # use only declared type signatures

function __init__()
    Base.REPL_MODULE_REF[] = REPL
end

answer_color(::AbstractREPL) = ""

const JULIA_PROMPT = "julia> "
const PKG_PROMPT = "pkg> "
const SHELL_PROMPT = "shell> "
const HELP_PROMPT = "help?> "

mutable struct REPLBackend
    "channel for AST"
    repl_channel::Channel{Any}
    "channel for results: (value, iserror)"
    response_channel::Channel{Any}
    "flag indicating the state of this backend"
    in_eval::Bool
    "transformation functions to apply before evaluating expressions"
    ast_transforms::Vector{Any}
    "current backend task"
    backend_task::Task

    REPLBackend(repl_channel, response_channel, in_eval, ast_transforms=copy(repl_ast_transforms)) =
        new(repl_channel, response_channel, in_eval, ast_transforms)
end
REPLBackend() = REPLBackend(Channel(1), Channel(1), false)

"""
    softscope(ex)

Return a modified version of the parsed expression `ex` that uses
the REPL's "soft" scoping rules for global syntax blocks.
"""
function softscope(@nospecialize ex)
    if ex isa Expr
        h = ex.head
        if h === :toplevel
            ex′ = Expr(h)
            map!(softscope, resize!(ex′.args, length(ex.args)), ex.args)
            return ex′
        elseif h in (:meta, :import, :using, :export, :module, :error, :incomplete, :thunk)
            return ex
        elseif h === :global && all(x->isa(x, Symbol), ex.args)
            return ex
        else
            return Expr(:block, Expr(:softscope, true), ex)
        end
    end
    return ex
end

# Temporary alias until Documenter updates
const softscope! = softscope

const repl_ast_transforms = Any[softscope] # defaults for new REPL backends

# Allows an external package to add hooks into the code loading.
# The hook should take a Vector{Symbol} of package names and
# return true if all packages could be installed, false if not
# to e.g. install packages on demand
const install_packages_hooks = Any[]

function eval_user_input(@nospecialize(ast), backend::REPLBackend)
    lasterr = nothing
    Base.sigatomic_begin()
    while true
        try
            Base.sigatomic_end()
            if lasterr !== nothing
                put!(backend.response_channel, Pair{Any, Bool}(lasterr, true))
            else
                backend.in_eval = true
                if !isempty(install_packages_hooks)
                    check_for_missing_packages_and_run_hooks(ast)
                end
                for xf in backend.ast_transforms
                    ast = Base.invokelatest(xf, ast)
                end
                value = Core.eval(Main, ast)
                backend.in_eval = false
                setglobal!(Main, :ans, value)
                put!(backend.response_channel, Pair{Any, Bool}(value, false))
            end
            break
        catch err
            if lasterr !== nothing
                println("SYSTEM ERROR: Failed to report error to REPL frontend")
                println(err)
            end
            lasterr = current_exceptions()
        end
    end
    Base.sigatomic_end()
    nothing
end

function check_for_missing_packages_and_run_hooks(ast)
    isa(ast, Expr) || return
    mods = modules_to_be_loaded(ast)
    filter!(mod -> isnothing(Base.identify_package(String(mod))), mods) # keep missing modules
    if !isempty(mods)
        for f in install_packages_hooks
            Base.invokelatest(f, mods) && return
        end
    end
end

function modules_to_be_loaded(ast::Expr, mods::Vector{Symbol} = Symbol[])
    ast.head == :quote && return mods # don't search if it's not going to be run during this eval
    if ast.head in [:using, :import]
        for arg in ast.args
            arg = arg::Expr
            arg1 = first(arg.args)
            if arg1 isa Symbol # i.e. `Foo`
                if arg1 != :. # don't include local imports
                    push!(mods, arg1)
                end
            else # i.e. `Foo: bar`
                push!(mods, first((arg1::Expr).args))
            end
        end
    end
    for arg in ast.args
        if isexpr(arg, (:block, :if, :using, :import))
            modules_to_be_loaded(arg, mods)
        end
    end
    filter!(mod -> !in(String(mod), ["Base", "Main", "Core"]), mods) # Exclude special non-package modules
    return unique(mods)
end

"""
    start_repl_backend(repl_channel::Channel, response_channel::Channel)

    Starts loop for REPL backend
    Returns a REPLBackend with backend_task assigned

    Deprecated since sync / async behavior cannot be selected
"""
function start_repl_backend(repl_channel::Channel{Any}, response_channel::Channel{Any})
    # Maintain legacy behavior of asynchronous backend
    backend = REPLBackend(repl_channel, response_channel, false)
    # Assignment will be made twice, but will be immediately available
    backend.backend_task = @async start_repl_backend(backend)
    return backend
end

"""
    start_repl_backend(backend::REPLBackend)

    Call directly to run backend loop on current Task.
    Use @async for run backend on new Task.

    Does not return backend until loop is finished.
"""
function start_repl_backend(backend::REPLBackend,  @nospecialize(consumer = x -> nothing))
    backend.backend_task = Base.current_task()
    consumer(backend)
    repl_backend_loop(backend)
    return backend
end

function repl_backend_loop(backend::REPLBackend)
    # include looks at this to determine the relative include path
    # nothing means cwd
    while true
        tls = task_local_storage()
        tls[:SOURCE_PATH] = nothing
        ast, show_value = take!(backend.repl_channel)
        if show_value == -1
            # exit flag
            break
        end
        eval_user_input(ast, backend)
    end
    return nothing
end

struct REPLDisplay{R<:AbstractREPL} <: AbstractDisplay
    repl::R
end

==(a::REPLDisplay, b::REPLDisplay) = a.repl === b.repl

function display(d::REPLDisplay, mime::MIME"text/plain", x)
    x = Ref{Any}(x)
    with_repl_linfo(d.repl) do io
        io = IOContext(io, :limit => true, :module => Main::Module)
        get(io, :color, false) && write(io, answer_color(d.repl))
        if isdefined(d.repl, :options) && isdefined(d.repl.options, :iocontext)
            # this can override the :limit property set initially
            io = foldl(IOContext, d.repl.options.iocontext, init=io)
        end
        show(io, mime, x[])
        println(io)
    end
    return nothing
end
display(d::REPLDisplay, x) = display(d, MIME("text/plain"), x)

function print_response(repl::AbstractREPL, response, show_value::Bool, have_color::Bool)
    repl.waserror = response[2]
    with_repl_linfo(repl) do io
        io = IOContext(io, :module => Main::Module)
        print_response(io, response, show_value, have_color, specialdisplay(repl))
    end
    return nothing
end
function print_response(errio::IO, response, show_value::Bool, have_color::Bool, specialdisplay::Union{AbstractDisplay,Nothing}=nothing)
    Base.sigatomic_begin()
    val, iserr = response
    while true
        try
            Base.sigatomic_end()
            if iserr
                val = Base.scrub_repl_backtrace(val)
                Base.istrivialerror(val) || setglobal!(Main, :err, val)
                Base.invokelatest(Base.display_error, errio, val)
            else
                if val !== nothing && show_value
                    try
                        if specialdisplay === nothing
                            Base.invokelatest(display, val)
                        else
                            Base.invokelatest(display, specialdisplay, val)
                        end
                    catch
                        println(errio, "Error showing value of type ", typeof(val), ":")
                        rethrow()
                    end
                end
            end
            break
        catch ex
            if iserr
                println(errio) # an error during printing is likely to leave us mid-line
                println(errio, "SYSTEM (REPL): showing an error caused an error")
                try
                    excs = Base.scrub_repl_backtrace(current_exceptions())
                    setglobal!(Main, :err, excs)
                    Base.invokelatest(Base.display_error, errio, excs)
                catch e
                    # at this point, only print the name of the type as a Symbol to
                    # minimize the possibility of further errors.
                    println(errio)
                    println(errio, "SYSTEM (REPL): caught exception of type ", typeof(e).name.name,
                            " while trying to handle a nested exception; giving up")
                end
                break
            end
            val = current_exceptions()
            iserr = true
        end
    end
    Base.sigatomic_end()
    nothing
end

# A reference to a backend that is not mutable
struct REPLBackendRef
    repl_channel::Channel{Any}
    response_channel::Channel{Any}
end
REPLBackendRef(backend::REPLBackend) = REPLBackendRef(backend.repl_channel, backend.response_channel)
function destroy(ref::REPLBackendRef, state::Task)
    if istaskfailed(state)
        close(ref.repl_channel, TaskFailedException(state))
        close(ref.response_channel, TaskFailedException(state))
    end
    close(ref.repl_channel)
    close(ref.response_channel)
end

"""
    run_repl(repl::AbstractREPL)
    run_repl(repl, consumer = backend->nothing; backend_on_current_task = true)

    Main function to start the REPL

    consumer is an optional function that takes a REPLBackend as an argument
"""
function run_repl(repl::AbstractREPL, @nospecialize(consumer = x -> nothing); backend_on_current_task::Bool = true)
    backend = REPLBackend()
    backend_ref = REPLBackendRef(backend)
    cleanup = @task try
            destroy(backend_ref, t)
        catch e
            Core.print(Core.stderr, "\nINTERNAL ERROR: ")
            Core.println(Core.stderr, e)
            Core.println(Core.stderr, catch_backtrace())
        end
    if backend_on_current_task
        t = @async run_frontend(repl, backend_ref)
        errormonitor(t)
        Base._wait2(t, cleanup)
        start_repl_backend(backend, consumer)
    else
        t = @async start_repl_backend(backend, consumer)
        errormonitor(t)
        Base._wait2(t, cleanup)
        run_frontend(repl, backend_ref)
    end
    return backend
end

## BasicREPL ##

mutable struct BasicREPL <: AbstractREPL
    terminal::TextTerminal
    waserror::Bool
    BasicREPL(t) = new(t, false)
end

outstream(r::BasicREPL) = r.terminal
hascolor(r::BasicREPL) = hascolor(r.terminal)

function run_frontend(repl::BasicREPL, backend::REPLBackendRef)
    d = REPLDisplay(repl)
    dopushdisplay = !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    hit_eof = false
    while true
        Base.reseteof(repl.terminal)
        write(repl.terminal, JULIA_PROMPT)
        line = ""
        ast = nothing
        interrupted = false
        while true
            try
                line *= readline(repl.terminal, keep=true)
            catch e
                if isa(e,InterruptException)
                    try # raise the debugger if present
                        ccall(:jl_raise_debugger, Int, ())
                    catch
                    end
                    line = ""
                    interrupted = true
                    break
                elseif isa(e,EOFError)
                    hit_eof = true
                    break
                else
                    rethrow()
                end
            end
            ast = Base.parse_input_line(line)
            (isa(ast,Expr) && ast.head === :incomplete) || break
        end
        if !isempty(line)
            response = eval_with_backend(ast, backend)
            print_response(repl, response, !ends_with_semicolon(line), false)
        end
        write(repl.terminal, '\n')
        ((!interrupted && isempty(line)) || hit_eof) && break
    end
    # terminate backend
    put!(backend.repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
    nothing
end

## LineEditREPL ##

mutable struct LineEditREPL <: AbstractREPL
    t::TextTerminal
    hascolor::Bool
    prompt_color::String
    input_color::String
    answer_color::String
    shell_color::String
    help_color::String
    history_file::Bool
    in_shell::Bool
    in_help::Bool
    envcolors::Bool
    waserror::Bool
    specialdisplay::Union{Nothing,AbstractDisplay}
    options::Options
    mistate::Union{MIState,Nothing}
    last_shown_line_infos::Vector{Tuple{String,Int}}
    interface::ModalInterface
    backendref::REPLBackendRef
    function LineEditREPL(t,hascolor,prompt_color,input_color,answer_color,shell_color,help_color,history_file,in_shell,in_help,envcolors)
        opts = Options()
        opts.hascolor = hascolor
        if !hascolor
            opts.beep_colors = [""]
        end
        new(t,hascolor,prompt_color,input_color,answer_color,shell_color,help_color,history_file,in_shell,
            in_help,envcolors,false,nothing, opts, nothing, Tuple{String,Int}[])
    end
end
outstream(r::LineEditREPL) = r.t isa TTYTerminal ? r.t.out_stream : r.t
specialdisplay(r::LineEditREPL) = r.specialdisplay
specialdisplay(r::AbstractREPL) = nothing
terminal(r::LineEditREPL) = r.t
hascolor(r::LineEditREPL) = r.hascolor

LineEditREPL(t::TextTerminal, hascolor::Bool, envcolors::Bool=false) =
    LineEditREPL(t, hascolor,
        hascolor ? Base.text_colors[:green] : "",
        hascolor ? Base.input_color() : "",
        hascolor ? Base.answer_color() : "",
        hascolor ? Base.text_colors[:red] : "",
        hascolor ? Base.text_colors[:yellow] : "",
        false, false, false, envcolors
    )

mutable struct REPLCompletionProvider <: CompletionProvider
    modifiers::LineEdit.Modifiers
end
REPLCompletionProvider() = REPLCompletionProvider(LineEdit.Modifiers())
mutable struct ShellCompletionProvider <: CompletionProvider end
struct LatexCompletions <: CompletionProvider end

setmodifiers!(c::REPLCompletionProvider, m::LineEdit.Modifiers) = c.modifiers = m

beforecursor(buf::IOBuffer) = String(buf.data[1:buf.ptr-1])

function complete_line(c::REPLCompletionProvider, s::PromptState)
    partial = beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = completions(full, lastindex(partial), Main, c.modifiers.shift)
    c.modifiers = LineEdit.Modifiers()
    return unique!(map(completion_text, ret)), partial[range], should_complete
end

function complete_line(c::ShellCompletionProvider, s::PromptState)
    # First parse everything up to the current position
    partial = beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = shell_completions(full, lastindex(partial))
    return unique!(map(completion_text, ret)), partial[range], should_complete
end

function complete_line(c::LatexCompletions, s)
    partial = beforecursor(LineEdit.buffer(s))
    full = LineEdit.input_string(s)::String
    ret, range, should_complete = bslash_completions(full, lastindex(partial))[2]
    return unique!(map(completion_text, ret)), partial[range], should_complete
end

with_repl_linfo(f, repl) = f(outstream(repl))
function with_repl_linfo(f, repl::LineEditREPL)
    linfos = Tuple{String,Int}[]
    io = IOContext(outstream(repl), :last_shown_line_infos => linfos)
    f(io)
    if !isempty(linfos)
        repl.last_shown_line_infos = linfos
    end
    nothing
end

mutable struct REPLHistoryProvider <: HistoryProvider
    history::Vector{String}
    file_path::String
    history_file::Union{Nothing,IO}
    start_idx::Int
    cur_idx::Int
    last_idx::Int
    last_buffer::IOBuffer
    last_mode::Union{Nothing,Prompt}
    mode_mapping::Dict{Symbol,Prompt}
    modes::Vector{Symbol}
end
REPLHistoryProvider(mode_mapping::Dict{Symbol}) =
    REPLHistoryProvider(String[], "", nothing, 0, 0, -1, IOBuffer(),
                        nothing, mode_mapping, UInt8[])

invalid_history_message(path::String) = """
Invalid history file ($path) format:
If you have a history file left over from an older version of Julia,
try renaming or deleting it.
Invalid character: """

munged_history_message(path::String) = """
Invalid history file ($path) format:
An editor may have converted tabs to spaces at line """

function hist_open_file(hp::REPLHistoryProvider)
    f = open(hp.file_path, read=true, write=true, create=true)
    hp.history_file = f
    seekend(f)
end

function hist_from_file(hp::REPLHistoryProvider, path::String)
    getline(lines, i) = i > length(lines) ? "" : lines[i]
    file_lines = readlines(path)
    countlines = 0
    while true
        # First parse the metadata that starts with '#' in particular the REPL mode
        countlines += 1
        line = getline(file_lines, countlines)
        mode = :julia
        isempty(line) && break
        line[1] != '#' &&
            error(invalid_history_message(path), repr(line[1]), " at line ", countlines)
        while !isempty(line)
            startswith(line, '#') || break
            if startswith(line, "# mode: ")
                mode = Symbol(SubString(line, 9))
            end
            countlines += 1
            line = getline(file_lines, countlines)
        end
        isempty(line) && break

        # Now parse the code for the current REPL mode
        line[1] == ' '  &&
            error(munged_history_message(path), countlines)
        line[1] != '\t' &&
            error(invalid_history_message(path), repr(line[1]), " at line ", countlines)
        lines = String[]
        while !isempty(line)
            push!(lines, chomp(SubString(line, 2)))
            next_line = getline(file_lines, countlines+1)
            isempty(next_line) && break
            first(next_line) == ' '  && error(munged_history_message(path), countlines)
            # A line not starting with a tab means we are done with code for this entry
            first(next_line) != '\t' && break
            countlines += 1
            line = getline(file_lines, countlines)
        end
        push!(hp.modes, mode)
        push!(hp.history, join(lines, '\n'))
    end
    hp.start_idx = length(hp.history)
    return hp
end

function mode_idx(hist::REPLHistoryProvider, mode::TextInterface)
    c = :julia
    for (k,v) in hist.mode_mapping
        isequal(v, mode) && (c = k)
    end
    return c
end

function add_history(hist::REPLHistoryProvider, s::PromptState)
    str = rstrip(String(take!(copy(s.input_buffer))))
    isempty(strip(str)) && return
    mode = mode_idx(hist, LineEdit.mode(s))
    !isempty(hist.history) &&
        isequal(mode, hist.modes[end]) && str == hist.history[end] && return
    push!(hist.modes, mode)
    push!(hist.history, str)
    hist.history_file === nothing && return
    entry = """
    # time: $(Libc.strftime("%Y-%m-%d %H:%M:%S %Z", time()))
    # mode: $mode
    $(replace(str, r"^"ms => "\t"))
    """
    # TODO: write-lock history file
    try
        seekend(hist.history_file)
    catch err
        (err isa SystemError) || rethrow()
        # File handle might get stale after a while, especially under network file systems
        # If this doesn't fix it (e.g. when file is deleted), we'll end up rethrowing anyway
        hist_open_file(hist)
    end
    print(hist.history_file, entry)
    flush(hist.history_file)
    nothing
end

function history_move(s::Union{LineEdit.MIState,LineEdit.PrefixSearchState}, hist::REPLHistoryProvider, idx::Int, save_idx::Int = hist.cur_idx)
    max_idx = length(hist.history) + 1
    @assert 1 <= hist.cur_idx <= max_idx
    (1 <= idx <= max_idx) || return :none
    idx != hist.cur_idx || return :none

    # save the current line
    if save_idx == max_idx
        hist.last_mode = LineEdit.mode(s)
        hist.last_buffer = copy(LineEdit.buffer(s))
    else
        hist.history[save_idx] = LineEdit.input_string(s)
        hist.modes[save_idx] = mode_idx(hist, LineEdit.mode(s))
    end

    # load the saved line
    if idx == max_idx
        last_buffer = hist.last_buffer
        LineEdit.transition(s, hist.last_mode) do
            LineEdit.replace_line(s, last_buffer)
        end
        hist.last_mode = nothing
        hist.last_buffer = IOBuffer()
    else
        if haskey(hist.mode_mapping, hist.modes[idx])
            LineEdit.transition(s, hist.mode_mapping[hist.modes[idx]]) do
                LineEdit.replace_line(s, hist.history[idx])
            end
        else
            return :skip
        end
    end
    hist.cur_idx = idx

    return :ok
end

# REPL History can also transitions modes
function LineEdit.accept_result_newmode(hist::REPLHistoryProvider)
    if 1 <= hist.cur_idx <= length(hist.modes)
        return hist.mode_mapping[hist.modes[hist.cur_idx]]
    end
    return nothing
end

function history_prev(s::LineEdit.MIState, hist::REPLHistoryProvider,
                      num::Int=1, save_idx::Int = hist.cur_idx)
    num <= 0 && return history_next(s, hist, -num, save_idx)
    hist.last_idx = -1
    m = history_move(s, hist, hist.cur_idx-num, save_idx)
    if m === :ok
        LineEdit.move_input_start(s)
        LineEdit.reset_key_repeats(s) do
            LineEdit.move_line_end(s)
        end
        return LineEdit.refresh_line(s)
    elseif m === :skip
        return history_prev(s, hist, num+1, save_idx)
    else
        return Terminals.beep(s)
    end
end

function history_next(s::LineEdit.MIState, hist::REPLHistoryProvider,
                      num::Int=1, save_idx::Int = hist.cur_idx)
    if num == 0
        Terminals.beep(s)
        return
    end
    num < 0 && return history_prev(s, hist, -num, save_idx)
    cur_idx = hist.cur_idx
    max_idx = length(hist.history) + 1
    if cur_idx == max_idx && 0 < hist.last_idx
        # issue #6312
        cur_idx = hist.last_idx
        hist.last_idx = -1
    end
    m = history_move(s, hist, cur_idx+num, save_idx)
    if m === :ok
        LineEdit.move_input_end(s)
        return LineEdit.refresh_line(s)
    elseif m === :skip
        return history_next(s, hist, num+1, save_idx)
    else
        return Terminals.beep(s)
    end
end

history_first(s::LineEdit.MIState, hist::REPLHistoryProvider) =
    history_prev(s, hist, hist.cur_idx - 1 -
                 (hist.cur_idx > hist.start_idx+1 ? hist.start_idx : 0))

history_last(s::LineEdit.MIState, hist::REPLHistoryProvider) =
    history_next(s, hist, length(hist.history) - hist.cur_idx + 1)

function history_move_prefix(s::LineEdit.PrefixSearchState,
                             hist::REPLHistoryProvider,
                             prefix::AbstractString,
                             backwards::Bool,
                             cur_idx::Int = hist.cur_idx)
    cur_response = String(take!(copy(LineEdit.buffer(s))))
    # when searching forward, start at last_idx
    if !backwards && hist.last_idx > 0
        cur_idx = hist.last_idx
    end
    hist.last_idx = -1
    max_idx = length(hist.history)+1
    idxs = backwards ? ((cur_idx-1):-1:1) : ((cur_idx+1):1:max_idx)
    for idx in idxs
        if (idx == max_idx) || (startswith(hist.history[idx], prefix) && (hist.history[idx] != cur_response || get(hist.mode_mapping, hist.modes[idx], nothing) !== LineEdit.mode(s)))
            m = history_move(s, hist, idx)
            if m === :ok
                if idx == max_idx
                    # on resuming the in-progress edit, leave the cursor where the user last had it
                elseif isempty(prefix)
                    # on empty prefix search, move cursor to the end
                    LineEdit.move_input_end(s)
                else
                    # otherwise, keep cursor at the prefix position as a visual cue
                    seek(LineEdit.buffer(s), sizeof(prefix))
                end
                LineEdit.refresh_line(s)
                return :ok
            elseif m === :skip
                return history_move_prefix(s,hist,prefix,backwards,idx)
            end
        end
    end
    Terminals.beep(s)
    nothing
end
history_next_prefix(s::LineEdit.PrefixSearchState, hist::REPLHistoryProvider, prefix::AbstractString) =
    history_move_prefix(s, hist, prefix, false)
history_prev_prefix(s::LineEdit.PrefixSearchState, hist::REPLHistoryProvider, prefix::AbstractString) =
    history_move_prefix(s, hist, prefix, true)

function history_search(hist::REPLHistoryProvider, query_buffer::IOBuffer, response_buffer::IOBuffer,
                        backwards::Bool=false, skip_current::Bool=false)

    qpos = position(query_buffer)
    qpos > 0 || return true
    searchdata = beforecursor(query_buffer)
    response_str = String(take!(copy(response_buffer)))

    # Alright, first try to see if the current match still works
    a = position(response_buffer) + 1 # position is zero-indexed
    # FIXME: I'm pretty sure this is broken since it uses an index
    # into the search data to index into the response string
    b = a + sizeof(searchdata)
    b = b ≤ ncodeunits(response_str) ? prevind(response_str, b) : b-1
    b = min(lastindex(response_str), b) # ensure that b is valid

    searchstart = backwards ? b : a
    if searchdata == response_str[a:b]
        if skip_current
            searchstart = backwards ? prevind(response_str, b) : nextind(response_str, a)
        else
            return true
        end
    end

    # Start searching
    # First the current response buffer
    if 1 <= searchstart <= lastindex(response_str)
        match = backwards ? findprev(searchdata, response_str, searchstart) :
                            findnext(searchdata, response_str, searchstart)
        if match !== nothing
            seek(response_buffer, first(match) - 1)
            return true
        end
    end

    # Now search all the other buffers
    idxs = backwards ? ((hist.cur_idx-1):-1:1) : ((hist.cur_idx+1):1:length(hist.history))
    for idx in idxs
        h = hist.history[idx]
        match = backwards ? findlast(searchdata, h) : findfirst(searchdata, h)
        if match !== nothing && h != response_str && haskey(hist.mode_mapping, hist.modes[idx])
            truncate(response_buffer, 0)
            write(response_buffer, h)
            seek(response_buffer, first(match) - 1)
            hist.cur_idx = idx
            return true
        end
    end

    return false
end

function history_reset_state(hist::REPLHistoryProvider)
    if hist.cur_idx != length(hist.history) + 1
        hist.last_idx = hist.cur_idx
        hist.cur_idx = length(hist.history) + 1
    end
    nothing
end
LineEdit.reset_state(hist::REPLHistoryProvider) = history_reset_state(hist)

function return_callback(s)
    ast = Base.parse_input_line(String(take!(copy(LineEdit.buffer(s)))), depwarn=false)
    return !(isa(ast, Expr) && ast.head === :incomplete)
end

find_hist_file() = get(ENV, "JULIA_HISTORY",
                       !isempty(DEPOT_PATH) ? joinpath(DEPOT_PATH[1], "logs", "repl_history.jl") :
                       error("DEPOT_PATH is empty and and ENV[\"JULIA_HISTORY\"] not set."))

backend(r::AbstractREPL) = r.backendref

function eval_with_backend(ast, backend::REPLBackendRef)
    put!(backend.repl_channel, (ast, 1))
    return take!(backend.response_channel) # (val, iserr)
end

function respond(f, repl, main; pass_empty::Bool = false, suppress_on_semicolon::Bool = true)
    return function do_respond(s::MIState, buf, ok::Bool)
        if !ok
            return transition(s, :abort)
        end
        line = String(take!(buf)::Vector{UInt8})
        if !isempty(line) || pass_empty
            reset(repl)
            local response
            try
                ast = Base.invokelatest(f, line)
                response = eval_with_backend(ast, backend(repl))
            catch
                response = Pair{Any, Bool}(current_exceptions(), true)
            end
            hide_output = suppress_on_semicolon && ends_with_semicolon(line)
            print_response(repl, response, !hide_output, hascolor(repl))
        end
        prepare_next(repl)
        reset_state(s)
        return s.current_mode.sticky ? true : transition(s, main)
    end
end

function reset(repl::LineEditREPL)
    raw!(repl.t, false)
    hascolor(repl) && print(repl.t, Base.text_colors[:normal])
    nothing
end

function prepare_next(repl::LineEditREPL)
    println(terminal(repl))
end

function mode_keymap(julia_prompt::Prompt)
    AnyDict(
    '\b' => function (s::MIState,o...)
        if isempty(s) || position(LineEdit.buffer(s)) == 0
            buf = copy(LineEdit.buffer(s))
            transition(s, julia_prompt) do
                LineEdit.state(s, julia_prompt).input_buffer = buf
            end
        else
            LineEdit.edit_backspace(s)
        end
    end,
    "^C" => function (s::MIState,o...)
        LineEdit.move_input_end(s)
        LineEdit.refresh_line(s)
        print(LineEdit.terminal(s), "^C\n\n")
        transition(s, julia_prompt)
        transition(s, :reset)
        LineEdit.refresh_line(s)
    end)
end

repl_filename(repl, hp::REPLHistoryProvider) = "REPL[$(max(length(hp.history)-hp.start_idx, 1))]"
repl_filename(repl, hp) = "REPL"

const JL_PROMPT_PASTE = Ref(true)
enable_promptpaste(v::Bool) = JL_PROMPT_PASTE[] = v

setup_interface(
    repl::LineEditREPL;
    # those keyword arguments may be deprecated eventually in favor of the Options mechanism
    hascolor::Bool = repl.options.hascolor,
    extra_repl_keymap::Any = repl.options.extra_keymap
) = setup_interface(repl, hascolor, extra_repl_keymap)

# This non keyword method can be precompiled which is important
function setup_interface(
    repl::LineEditREPL,
    hascolor::Bool,
    extra_repl_keymap::Any, # Union{Dict,Vector{<:Dict}},
)
    # The precompile statement emitter has problem outputting valid syntax for the
    # type of `Union{Dict,Vector{<:Dict}}` (see #28808).
    # This function is however important to precompile for REPL startup time, therefore,
    # make the type Any and just assert that we have the correct type below.
    @assert extra_repl_keymap isa Union{Dict,Vector{<:Dict}}

    ###
    #
    # This function returns the main interface that describes the REPL
    # functionality, it is called internally by functions that setup a
    # Terminal-based REPL frontend.
    #
    # See run_frontend(repl::LineEditREPL, backend::REPLBackendRef)
    # for usage
    #
    ###

    ###
    # We setup the interface in two stages.
    # First, we set up all components (prompt,rsearch,shell,help)
    # Second, we create keymaps with appropriate transitions between them
    #   and assign them to the components
    #
    ###

    ############################### Stage I ################################

    # This will provide completions for REPL and help mode
    replc = REPLCompletionProvider()

    # Set up the main Julia prompt
    julia_prompt = Prompt(JULIA_PROMPT;
        # Copy colors from the prompt object
        prompt_prefix = hascolor ? repl.prompt_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        repl = repl,
        complete = replc,
        on_enter = return_callback)

    # Setup help mode
    help_mode = Prompt(HELP_PROMPT,
        prompt_prefix = hascolor ? repl.help_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        repl = repl,
        complete = replc,
        # When we're done transform the entered line into a call to helpmode function
        on_done = respond(line::String->helpmode(outstream(repl), line), repl, julia_prompt,
                          pass_empty=true, suppress_on_semicolon=false))


    # Set up shell mode
    shell_mode = Prompt(SHELL_PROMPT;
        prompt_prefix = hascolor ? repl.shell_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        repl = repl,
        complete = ShellCompletionProvider(),
        # Transform "foo bar baz" into `foo bar baz` (shell quoting)
        # and pass into Base.repl_cmd for processing (handles `ls` and `cd`
        # special)
        on_done = respond(repl, julia_prompt) do line
            Expr(:call, :(Base.repl_cmd),
                :(Base.cmd_gen($(Base.shell_parse(line::String)[1]))),
                outstream(repl))
        end,
        sticky = true)


    ################################# Stage II #############################

    # Setup history
    # We will have a unified history for all REPL modes
    hp = REPLHistoryProvider(Dict{Symbol,Prompt}(:julia => julia_prompt,
                                                 :shell => shell_mode,
                                                 :help  => help_mode))
    if repl.history_file
        try
            hist_path = find_hist_file()
            mkpath(dirname(hist_path))
            hp.file_path = hist_path
            hist_open_file(hp)
            finalizer(replc) do replc
                close(hp.history_file)
            end
            hist_from_file(hp, hist_path)
        catch
            # use REPL.hascolor to avoid using the local variable with the same name
            print_response(repl, Pair{Any, Bool}(current_exceptions(), true), true, REPL.hascolor(repl))
            println(outstream(repl))
            @info "Disabling history file for this session"
            repl.history_file = false
        end
    end
    history_reset_state(hp)
    julia_prompt.hist = hp
    shell_mode.hist = hp
    help_mode.hist = hp

    julia_prompt.on_done = respond(x->Base.parse_input_line(x,filename=repl_filename(repl,hp)), repl, julia_prompt)


    search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
    search_prompt.complete = LatexCompletions()

    jl_prompt_len = length(JULIA_PROMPT)
    pkg_prompt_len = length(PKG_PROMPT)
    shell_prompt_len = length(SHELL_PROMPT)
    help_prompt_len = length(HELP_PROMPT)
    pkg_prompt_regex = r"^(?:\(.+\) )?pkg> "

    # Canonicalize user keymap input
    if isa(extra_repl_keymap, Dict)
        extra_repl_keymap = AnyDict[extra_repl_keymap]
    end

    repl_keymap = AnyDict(
        ';' => function (s::MIState,o...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, shell_mode) do
                    LineEdit.state(s, shell_mode).input_buffer = buf
                end
            else
                edit_insert(s, ';')
            end
        end,
        '?' => function (s::MIState,o...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, help_mode) do
                    LineEdit.state(s, help_mode).input_buffer = buf
                end
            else
                edit_insert(s, '?')
            end
        end,

        # Bracketed Paste Mode
        "\e[200~" => (s::MIState,o...)->begin
            input = LineEdit.bracketed_paste(s) # read directly from s until reaching the end-bracketed-paste marker
            sbuffer = LineEdit.buffer(s)
            curspos = position(sbuffer)
            seek(sbuffer, 0)
            shouldeval = (bytesavailable(sbuffer) == curspos && !occursin(UInt8('\n'), sbuffer))
            seek(sbuffer, curspos)
            if curspos == 0
                # if pasting at the beginning, strip leading whitespace
                input = lstrip(input)
            end
            if !shouldeval
                # when pasting in the middle of input, just paste in place
                # don't try to execute all the WIP, since that's rather confusing
                # and is often ill-defined how it should behave
                edit_insert(s, input)
                return
            end
            LineEdit.push_undo(s)
            edit_insert(sbuffer, input)
            input = String(take!(sbuffer))
            oldpos = firstindex(input)
            firstline = true
            isprompt_paste = false
            curr_prompt_len = 0
            pasting_help = false

            while oldpos <= lastindex(input) # loop until all lines have been executed
                if JL_PROMPT_PASTE[]
                    # Check if the next statement starts with a prompt i.e. "julia> ", in that case
                    # skip it. But first skip whitespace unless pasting in a docstring which may have
                    # indented prompt examples that we don't want to execute
                    while input[oldpos] in (pasting_help ? ('\n') : ('\n', ' ', '\t'))
                        oldpos = nextind(input, oldpos)
                        oldpos >= sizeof(input) && return
                    end
                    # Check if input line starts with "julia> ", remove it if we are in prompt paste mode
                    if (firstline || isprompt_paste) && startswith(SubString(input, oldpos), JULIA_PROMPT)
                        isprompt_paste = true
                        oldpos += jl_prompt_len
                        curr_prompt_len = jl_prompt_len
                        transition(s, julia_prompt)
                        pasting_help = false
                    # Check if input line starts with "pkg> " or "(...) pkg> ", remove it if we are in prompt paste mode and switch mode
                    elseif (firstline || isprompt_paste) && startswith(SubString(input, oldpos), pkg_prompt_regex)
                        detected_pkg_prompt = match(pkg_prompt_regex, SubString(input, oldpos)).match
                        isprompt_paste = true
                        curr_prompt_len = sizeof(detected_pkg_prompt)
                        oldpos += curr_prompt_len
                        Base.active_repl.interface.modes[1].keymap_dict[']'](s, o...)
                        pasting_help = false
                    # Check if input line starts with "shell> ", remove it if we are in prompt paste mode and switch mode
                    elseif (firstline || isprompt_paste) && startswith(SubString(input, oldpos), SHELL_PROMPT)
                        isprompt_paste = true
                        oldpos += shell_prompt_len
                        curr_prompt_len = shell_prompt_len
                        transition(s, shell_mode)
                        pasting_help = false
                    # Check if input line starts with "help?> ", remove it if we are in prompt paste mode and switch mode
                    elseif (firstline || isprompt_paste) && startswith(SubString(input, oldpos), HELP_PROMPT)
                        isprompt_paste = true
                        oldpos += help_prompt_len
                        curr_prompt_len = help_prompt_len
                        transition(s, help_mode)
                        pasting_help = true
                    # If we are prompt pasting and current statement does not begin with a mode prefix, skip to next line
                    elseif isprompt_paste
                        while input[oldpos] != '\n'
                            oldpos = nextind(input, oldpos)
                            oldpos >= sizeof(input) && return
                        end
                        continue
                    end
                end
                dump_tail = false
                nl_pos = findfirst('\n', input[oldpos:end])
                if s.current_mode == julia_prompt
                    ast, pos = Meta.parse(input, oldpos, raise=false, depwarn=false)
                    if (isa(ast, Expr) && (ast.head === :error || ast.head === :incomplete)) ||
                            (pos > ncodeunits(input) && !endswith(input, '\n'))
                        # remaining text is incomplete (an error, or parser ran to the end but didn't stop with a newline):
                        # Insert all the remaining text as one line (might be empty)
                        dump_tail = true
                    end
                elseif isnothing(nl_pos) # no newline at end, so just dump the tail into the prompt and don't execute
                    dump_tail = true
                elseif s.current_mode == shell_mode # handle multiline shell commands
                    lines = split(input[oldpos:end], '\n')
                    pos = oldpos + sizeof(lines[1]) + 1
                    if length(lines) > 1
                        for line in lines[2:end]
                            # to be recognized as a multiline shell command, the lines must be indented to the
                            # same prompt position
                            if !startswith(line, ' '^curr_prompt_len)
                                break
                            end
                            pos += sizeof(line) + 1
                        end
                    end
                else
                    pos = oldpos + nl_pos
                end
                if dump_tail
                    tail = input[oldpos:end]
                    if !firstline
                        # strip leading whitespace, but only if it was the result of executing something
                        # (avoids modifying the user's current leading wip line)
                        tail = lstrip(tail)
                    end
                    if isprompt_paste # remove indentation spaces corresponding to the prompt
                        tail = replace(tail, r"^"m * ' '^curr_prompt_len => "")
                    end
                    LineEdit.replace_line(s, tail, true)
                    LineEdit.refresh_line(s)
                    break
                end
                # get the line and strip leading and trailing whitespace
                line = strip(input[oldpos:prevind(input, pos)])
                if !isempty(line)
                    if isprompt_paste # remove indentation spaces corresponding to the prompt
                        line = replace(line, r"^"m * ' '^curr_prompt_len => "")
                    end
                    # put the line on the screen and history
                    LineEdit.replace_line(s, line)
                    LineEdit.commit_line(s)
                    # execute the statement
                    terminal = LineEdit.terminal(s) # This is slightly ugly but ok for now
                    raw!(terminal, false) && disable_bracketed_paste(terminal)
                    LineEdit.mode(s).on_done(s, LineEdit.buffer(s), true)
                    raw!(terminal, true) && enable_bracketed_paste(terminal)
                    LineEdit.push_undo(s) # when the last line is incomplete
                end
                oldpos = pos
                firstline = false
            end
        end,

        # Open the editor at the location of a stackframe or method
        # This is accessing a contextual variable that gets set in
        # the show_backtrace and show_method_table functions.
        "^Q" => (s::MIState, o...) -> begin
            linfos = repl.last_shown_line_infos
            str = String(take!(LineEdit.buffer(s)))
            n = tryparse(Int, str)
            n === nothing && @goto writeback
            if n <= 0 || n > length(linfos) || startswith(linfos[n][1], "REPL[")
                @goto writeback
            end
            try
                InteractiveUtils.edit(linfos[n][1], linfos[n][2])
            catch ex
                ex isa ProcessFailedException || ex isa Base.IOError || ex isa SystemError || rethrow()
                @info "edit failed" _exception=ex
            end
            LineEdit.refresh_line(s)
            return
            @label writeback
            write(LineEdit.buffer(s), str)
            return
        end,
    )

    prefix_prompt, prefix_keymap = LineEdit.setup_prefix_keymap(hp, julia_prompt)

    a = Dict{Any,Any}[skeymap, repl_keymap, prefix_keymap, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    prepend!(a, extra_repl_keymap)

    julia_prompt.keymap_dict = LineEdit.keymap(a)

    mk = mode_keymap(julia_prompt)

    b = Dict{Any,Any}[skeymap, mk, prefix_keymap, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
    prepend!(b, extra_repl_keymap)

    shell_mode.keymap_dict = help_mode.keymap_dict = LineEdit.keymap(b)

    allprompts = LineEdit.TextInterface[julia_prompt, shell_mode, help_mode, search_prompt, prefix_prompt]
    return ModalInterface(allprompts)
end

function run_frontend(repl::LineEditREPL, backend::REPLBackendRef)
    d = REPLDisplay(repl)
    dopushdisplay = repl.specialdisplay === nothing && !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    if !isdefined(repl,:interface)
        interface = repl.interface = setup_interface(repl)
    else
        interface = repl.interface
    end
    repl.backendref = backend
    repl.mistate = LineEdit.init_state(terminal(repl), interface)
    run_interface(terminal(repl), interface, repl.mistate)
    # Terminate Backend
    put!(backend.repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
    nothing
end

## StreamREPL ##

mutable struct StreamREPL <: AbstractREPL
    stream::IO
    prompt_color::String
    input_color::String
    answer_color::String
    waserror::Bool
    StreamREPL(stream,pc,ic,ac) = new(stream,pc,ic,ac,false)
end
StreamREPL(stream::IO) = StreamREPL(stream, Base.text_colors[:green], Base.input_color(), Base.answer_color())
run_repl(stream::IO) = run_repl(StreamREPL(stream))

outstream(s::StreamREPL) = s.stream
hascolor(s::StreamREPL) = get(s.stream, :color, false)::Bool

answer_color(r::LineEditREPL) = r.envcolors ? Base.answer_color() : r.answer_color
answer_color(r::StreamREPL) = r.answer_color
input_color(r::LineEditREPL) = r.envcolors ? Base.input_color() : r.input_color
input_color(r::StreamREPL) = r.input_color

let matchend = Dict("\"" => r"\"", "\"\"\"" => r"\"\"\"", "'" => r"'",
    "`" => r"`", "```" => r"```", "#" => r"$"m, "#=" => r"=#|#=")
    global _rm_strings_and_comments
    function _rm_strings_and_comments(code::Union{String,SubString{String}})
        buf = IOBuffer(sizehint = sizeof(code))
        pos = 1
        while true
            i = findnext(r"\"(?!\"\")|\"\"\"|'|`(?!``)|```|#(?!=)|#=", code, pos)
            isnothing(i) && break
            match = SubString(code, i)
            j = findnext(matchend[match]::Regex, code, nextind(code, last(i)))
            if match == "#=" # possibly nested
                nested = 1
                while j !== nothing
                    nested += SubString(code, j) == "#=" ? +1 : -1
                    iszero(nested) && break
                    j = findnext(r"=#|#=", code, nextind(code, last(j)))
                end
            elseif match[1] != '#' # quote match: check non-escaped
                while j !== nothing
                    notbackslash = findprev(!=('\\'), code, prevind(code, first(j)))::Int
                    isodd(first(j) - notbackslash) && break # not escaped
                    j = findnext(matchend[match]::Regex, code, nextind(code, first(j)))
                end
            end
            isnothing(j) && break
            if match[1] == '#'
                print(buf, SubString(code, pos, prevind(code, first(i))))
            else
                print(buf, SubString(code, pos, last(i)), ' ', SubString(code, j))
            end
            pos = nextind(code, last(j))
        end
        print(buf, SubString(code, pos, lastindex(code)))
        return String(take!(buf))
    end
end

# heuristic function to decide if the presence of a semicolon
# at the end of the expression was intended for suppressing output
ends_with_semicolon(code::AbstractString) = ends_with_semicolon(String(code))
ends_with_semicolon(code::Union{String,SubString{String}}) =
    contains(_rm_strings_and_comments(code), r";\s*$")

function run_frontend(repl::StreamREPL, backend::REPLBackendRef)
    have_color = hascolor(repl)
    Base.banner(repl.stream)
    d = REPLDisplay(repl)
    dopushdisplay = !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    while !eof(repl.stream)::Bool
        if have_color
            print(repl.stream,repl.prompt_color)
        end
        print(repl.stream, "julia> ")
        if have_color
            print(repl.stream, input_color(repl))
        end
        line = readline(repl.stream, keep=true)
        if !isempty(line)
            ast = Base.parse_input_line(line)
            if have_color
                print(repl.stream, Base.color_normal)
            end
            response = eval_with_backend(ast, backend)
            print_response(repl, response, !ends_with_semicolon(line), have_color)
        end
    end
    # Terminate Backend
    put!(backend.repl_channel, (nothing, -1))
    dopushdisplay && popdisplay(d)
    nothing
end

end # module
