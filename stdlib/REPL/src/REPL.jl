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
    ==,
    catch_stack


include("Terminals.jl")
using .Terminals

abstract type AbstractREPL end

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
    terminal,
    MIState

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

mutable struct REPLBackend
    "channel for AST"
    repl_channel::Channel
    "channel for results: (value, iserror)"
    response_channel::Channel
    "flag indicating the state of this backend"
    in_eval::Bool
    "transformation functions to apply before evaluating expressions"
    ast_transforms::Vector{Any}
    "current backend task"
    backend_task::Task

    REPLBackend(repl_channel, response_channel, in_eval, ast_transforms=copy(repl_ast_transforms)) =
        new(repl_channel, response_channel, in_eval, ast_transforms)
end
REPLBackend() = REPLBackend(Channel(1),Channel(1),false)

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

function eval_user_input(@nospecialize(ast), backend::REPLBackend)
    lasterr = nothing
    Base.sigatomic_begin()
    while true
        try
            Base.sigatomic_end()
            if lasterr !== nothing
                put!(backend.response_channel, (lasterr,true))
            else
                backend.in_eval = true
                for xf in backend.ast_transforms
                    ast = Base.invokelatest(xf, ast)
                end
                value = Core.eval(Main, ast)
                backend.in_eval = false
                # note: use jl_set_global to make sure value isn't passed through `expand`
                ccall(:jl_set_global, Cvoid, (Any, Any, Any), Main, :ans, value)
                put!(backend.response_channel, (value,false))
            end
            break
        catch err
            if lasterr !== nothing
                println("SYSTEM ERROR: Failed to report error to REPL frontend")
                println(err)
            end
            lasterr = catch_stack()
        end
    end
    Base.sigatomic_end()
    nothing
end

"""
    start_repl_backend(repl_channel::Channel,response_channel::Channel)

    Starts loop for REPL backend
    Returns a REPLBackend with backend_task assigned

    Deprecated since sync / async behavior cannot be selected
"""
function start_repl_backend(repl_channel::Channel, response_channel::Channel)
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
    nothing
end

struct REPLDisplay{R<:AbstractREPL} <: AbstractDisplay
    repl::R
end

==(a::REPLDisplay, b::REPLDisplay) = a.repl === b.repl

function display(d::REPLDisplay, mime::MIME"text/plain", x)
    with_methodtable_hint(d.repl) do io
        io = IOContext(io, :limit => true, :module => Main)
        get(io, :color, false) && write(io, answer_color(d.repl))
        if isdefined(d.repl, :options) && isdefined(d.repl.options, :iocontext)
            # this can override the :limit property set initially
            io = foldl(IOContext, d.repl.options.iocontext, init=io)
        end
        show(io, mime, x)
        println(io)
    end
    nothing
end
display(d::REPLDisplay, x) = display(d, MIME("text/plain"), x)

function print_response(repl::AbstractREPL, @nospecialize(response), show_value::Bool, have_color::Bool)
    repl.waserror = response[2]
    with_methodtable_hint(repl) do io
        io = IOContext(io, :module => Main)
        print_response(io, response, show_value, have_color, specialdisplay(repl))
    end
    nothing
end
function print_response(errio::IO, @nospecialize(response), show_value::Bool, have_color::Bool, specialdisplay=nothing)
    Base.sigatomic_begin()
    val, iserr = response
    while true
        try
            Base.sigatomic_end()
            if iserr
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
        catch
            if iserr
                println(errio) # an error during printing is likely to leave us mid-line
                println(errio, "SYSTEM (REPL): showing an error caused an error")
                try
                    Base.invokelatest(Base.display_error, errio, catch_stack())
                catch e
                    # at this point, only print the name of the type as a Symbol to
                    # minimize the possibility of further errors.
                    println(errio)
                    println(errio, "SYSTEM (REPL): caught exception of type ", typeof(e).name.name,
                            " while trying to handle a nested exception; giving up")
                end
                break
            end
            val = catch_stack()
            iserr = true
        end
    end
    Base.sigatomic_end()
    nothing
end

# A reference to a backend that is not mutable
struct REPLBackendRef
    repl_channel::Channel
    response_channel::Channel
end
REPLBackendRef(backend::REPLBackend) = REPLBackendRef(backend.repl_channel,backend.response_channel)

"""
    run_repl(repl::AbstractREPL)
    run_repl(repl, consumer = backend->nothing; backend_on_current_task = true)

    Main function to start the REPL

    consumer is an optional function that takes a REPLBackend as an argument
"""
function run_repl(repl::AbstractREPL, @nospecialize(consumer = x -> nothing); backend_on_current_task::Bool = true)
    backend = REPLBackend()
    backend_ref = REPLBackendRef(backend)
    if backend_on_current_task
        @async run_frontend(repl, backend_ref)
        start_repl_backend(backend,consumer)
    else
        @async start_repl_backend(backend,consumer)
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

## User Options

mutable struct Options
    hascolor::Bool
    extra_keymap::Union{Dict,Vector{<:Dict}}
    # controls the presumed tab width of code pasted into the REPL.
    # Must satisfy `0 < tabwidth <= 16`.
    tabwidth::Int
    # Maximum number of entries in the kill ring queue.
    # Beyond this number, oldest entries are discarded first.
    kill_ring_max::Int
    region_animation_duration::Float64
    beep_duration::Float64
    beep_blink::Float64
    beep_maxduration::Float64
    beep_colors::Vector{String}
    beep_use_current::Bool
    backspace_align::Bool
    backspace_adjust::Bool
    confirm_exit::Bool # ^D must be repeated to confirm exit
    auto_indent::Bool # indent a newline like line above
    auto_indent_tmp_off::Bool # switch auto_indent temporarily off if copy&paste
    auto_indent_bracketed_paste::Bool # set to true if terminal knows paste mode
    # cancel auto-indent when next character is entered within this time frame :
    auto_indent_time_threshold::Float64
    # default IOContext settings at the REPL
    iocontext::Dict{Symbol,Any}
end

Options(;
        hascolor = true,
        extra_keymap = AnyDict[],
        tabwidth = 8,
        kill_ring_max = 100,
        region_animation_duration = 0.2,
        beep_duration = 0.2, beep_blink = 0.2, beep_maxduration = 1.0,
        beep_colors = ["\e[90m"], # gray (text_colors not yet available)
        beep_use_current = true,
        backspace_align = true, backspace_adjust = backspace_align,
        confirm_exit = false,
        auto_indent = true,
        auto_indent_tmp_off = false,
        auto_indent_bracketed_paste = false,
        auto_indent_time_threshold = 0.005,
        iocontext = Dict{Symbol,Any}()) =
            Options(hascolor, extra_keymap, tabwidth,
                    kill_ring_max, region_animation_duration,
                    beep_duration, beep_blink, beep_maxduration,
                    beep_colors, beep_use_current,
                    backspace_align, backspace_adjust, confirm_exit,
                    auto_indent, auto_indent_tmp_off, auto_indent_bracketed_paste,
                    auto_indent_time_threshold,
                    iocontext)

# for use by REPLs not having an options field
const GlobalOptions = Options()


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
    LineEditREPL(t,hascolor,prompt_color,input_color,answer_color,shell_color,help_color,history_file,in_shell,in_help,envcolors) =
        new(t,hascolor,prompt_color,input_color,answer_color,shell_color,help_color,history_file,in_shell,
            in_help,envcolors,false,nothing, Options(), nothing, Tuple{String,Int}[])
end
outstream(r::LineEditREPL) = r.t
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

mutable struct REPLCompletionProvider <: CompletionProvider end
mutable struct ShellCompletionProvider <: CompletionProvider end
struct LatexCompletions <: CompletionProvider end

beforecursor(buf::IOBuffer) = String(buf.data[1:buf.ptr-1])

function complete_line(c::REPLCompletionProvider, s)
    partial = beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = completions(full, lastindex(partial))
    return unique!(map(completion_text, ret)), partial[range], should_complete
end

function complete_line(c::ShellCompletionProvider, s)
    # First parse everything up to the current position
    partial = beforecursor(s.input_buffer)
    full = LineEdit.input_string(s)
    ret, range, should_complete = shell_completions(full, lastindex(partial))
    return unique!(map(completion_text, ret)), partial[range], should_complete
end

function complete_line(c::LatexCompletions, s)
    partial = beforecursor(LineEdit.buffer(s))
    full = LineEdit.input_string(s)
    ret, range, should_complete = bslash_completions(full, lastindex(partial))[2]
    return unique!(map(completion_text, ret)), partial[range], should_complete
end

with_methodtable_hint(f, repl) = f(outstream(repl))
function with_methodtable_hint(f, repl::LineEditREPL)
    linfos = Tuple{String,Int}[]
    io = IOContext(outstream(repl), :last_shown_line_infos => linfos)
    f(io)
    if !isempty(linfos)
        repl.last_shown_line_infos = linfos
        println(
            io,
            "\nTo edit a specific method, type the corresponding number into the " *
            "REPL and press Ctrl+Q",
        )
    end
    nothing
end

mutable struct REPLHistoryProvider <: HistoryProvider
    history::Vector{String}
    history_file::Union{Nothing,IO}
    start_idx::Int
    cur_idx::Int
    last_idx::Int
    last_buffer::IOBuffer
    last_mode::Union{Nothing,Prompt}
    mode_mapping::Dict{Symbol,Prompt}
    modes::Vector{Symbol}
end
REPLHistoryProvider(mode_mapping) =
    REPLHistoryProvider(String[], nothing, 0, 0, -1, IOBuffer(),
                        nothing, mode_mapping, UInt8[])

invalid_history_message(path::String) = """
Invalid history file ($path) format:
If you have a history file left over from an older version of Julia,
try renaming or deleting it.
Invalid character: """

munged_history_message(path::String) = """
Invalid history file ($path) format:
An editor may have converted tabs to spaces at line """

function hist_getline(file)
    while !eof(file)
        line = readline(file, keep=true)
        isempty(line) && return line
        line[1] in "\r\n" || return line
    end
    return ""
end

function hist_from_file(hp, file, path)
    hp.history_file = file
    seek(file, 0)
    countlines = 0
    while true
        mode = :julia
        line = hist_getline(file)
        isempty(line) && break
        countlines += 1
        line[1] != '#' &&
            error(invalid_history_message(path), repr(line[1]), " at line ", countlines)
        while !isempty(line)
            m = match(r"^#\s*(\w+)\s*:\s*(.*?)\s*$", line)
            m === nothing && break
            if m.captures[1] == "mode"
                mode = Symbol(m.captures[2])
            end
            line = hist_getline(file)
            countlines += 1
        end
        isempty(line) && break
        # Make sure starts with tab
        line[1] == ' '  &&
            error(munged_history_message(path), countlines)
        line[1] != '\t' &&
            error(invalid_history_message(path), repr(line[1]), " at line ", countlines)
        lines = String[]
        while !isempty(line)
            push!(lines, chomp(line[2:end]))
            eof(file) && break
            ch = peek(file, Char)
            ch == ' '  && error(munged_history_message(path), countlines)
            ch != '\t' && break
            line = hist_getline(file)
            countlines += 1
        end
        push!(hp.modes, mode)
        push!(hp.history, join(lines, '\n'))
    end
    seekend(file)
    hp.start_idx = length(hp.history)
    return hp
end

function mode_idx(hist::REPLHistoryProvider, mode)
    c = :julia
    for (k,v) in hist.mode_mapping
        isequal(v, mode) && (c = k)
    end
    return c
end

function add_history(hist::REPLHistoryProvider, s)
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
    seekend(hist.history_file)
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
                             cur_idx = hist.cur_idx)
    cur_response = String(take!(copy(LineEdit.buffer(s))))
    # when searching forward, start at last_idx
    if !backwards && hist.last_idx > 0
        cur_idx = hist.last_idx
    end
    hist.last_idx = -1
    max_idx = length(hist.history)+1
    idxs = backwards ? ((cur_idx-1):-1:1) : ((cur_idx+1):max_idx)
    for idx in idxs
        if (idx == max_idx) || (startswith(hist.history[idx], prefix) && (hist.history[idx] != cur_response || hist.modes[idx] != LineEdit.mode(s)))
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

    searchfunc1, searchfunc2, searchstart, skipfunc = backwards ?
                                                      (findlast, findprev, b, prevind) :
                                                      (findfirst, findnext, a, nextind)

    if searchdata == response_str[a:b]
        if skip_current
            searchstart = skipfunc(response_str, searchstart)
        else
            return true
        end
    end

    # Start searching
    # First the current response buffer
    if 1 <= searchstart <= lastindex(response_str)
        match = searchfunc2(searchdata, response_str, searchstart)
        if match !== nothing
            seek(response_buffer, first(match) - 1)
            return true
        end
    end

    # Now search all the other buffers
    idxs = backwards ? ((hist.cur_idx-1):-1:1) : ((hist.cur_idx+1):length(hist.history))
    for idx in idxs
        h = hist.history[idx]
        match = searchfunc1(searchdata, h)
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
    take!(backend.response_channel) # (val, iserr)
end

function respond(f, repl, main; pass_empty = false, suppress_on_semicolon = true)
    return function do_respond(s, buf, ok)
        if !ok
            return transition(s, :abort)
        end
        line = String(take!(buf))
        if !isempty(line) || pass_empty
            reset(repl)
            local response
            try
                ast = Base.invokelatest(f, line)
                response = eval_with_backend(ast, backend(repl))
            catch
                response = (catch_stack(), true)
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
    print(repl.t, Base.text_colors[:normal])
end

function prepare_next(repl::LineEditREPL)
    println(terminal(repl))
end

function mode_keymap(julia_prompt::Prompt)
    AnyDict(
    '\b' => function (s,o...)
        if isempty(s) || position(LineEdit.buffer(s)) == 0
            buf = copy(LineEdit.buffer(s))
            transition(s, julia_prompt) do
                LineEdit.state(s, julia_prompt).input_buffer = buf
            end
        else
            LineEdit.edit_backspace(s)
        end
    end,
    "^C" => function (s,o...)
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
    help_mode = Prompt("help?> ",
        prompt_prefix = hascolor ? repl.help_color : "",
        prompt_suffix = hascolor ?
            (repl.envcolors ? Base.input_color : repl.input_color) : "",
        repl = repl,
        complete = replc,
        # When we're done transform the entered line into a call to helpmode function
        on_done = respond(line->helpmode(outstream(repl), line), repl, julia_prompt,
                          pass_empty=true, suppress_on_semicolon=false))


    # Set up shell mode
    shell_mode = Prompt("shell> ";
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
                :(Base.cmd_gen($(Base.shell_parse(line)[1]))),
                outstream(repl))
        end)


    ################################# Stage II #############################

    # Setup history
    # We will have a unified history for all REPL modes
    hp = REPLHistoryProvider(Dict{Symbol,Any}(:julia => julia_prompt,
                                              :shell => shell_mode,
                                              :help  => help_mode))
    if repl.history_file
        try
            hist_path = find_hist_file()
            mkpath(dirname(hist_path))
            f = open(hist_path, read=true, write=true, create=true)
            finalizer(replc) do replc
                close(f)
            end
            hist_from_file(hp, f, hist_path)
        catch
            print_response(repl, (catch_stack(),true), true, hascolor(repl))
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

    # Canonicalize user keymap input
    if isa(extra_repl_keymap, Dict)
        extra_repl_keymap = [extra_repl_keymap]
    end

    repl_keymap = AnyDict(
        ';' => function (s,o...)
            if isempty(s) || position(LineEdit.buffer(s)) == 0
                buf = copy(LineEdit.buffer(s))
                transition(s, shell_mode) do
                    LineEdit.state(s, shell_mode).input_buffer = buf
                end
            else
                edit_insert(s, ';')
            end
        end,
        '?' => function (s,o...)
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
        "\e[200~" => (s,o...)->begin
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
            jl_prompt_len = 7 # "julia> "
            while oldpos <= lastindex(input) # loop until all lines have been executed
                if JL_PROMPT_PASTE[]
                    # Check if the next statement starts with "julia> ", in that case
                    # skip it. But first skip whitespace
                    while input[oldpos] in ('\n', ' ', '\t')
                        oldpos = nextind(input, oldpos)
                        oldpos >= sizeof(input) && return
                    end
                    # Check if input line starts with "julia> ", remove it if we are in prompt paste mode
                    if (firstline || isprompt_paste) && startswith(SubString(input, oldpos), JULIA_PROMPT)
                        isprompt_paste = true
                        oldpos += jl_prompt_len
                    # If we are prompt pasting and current statement does not begin with julia> , skip to next line
                    elseif isprompt_paste
                        while input[oldpos] != '\n'
                            oldpos = nextind(input, oldpos)
                            oldpos >= sizeof(input) && return
                        end
                        continue
                    end
                end
                ast, pos = Meta.parse(input, oldpos, raise=false, depwarn=false)
                if (isa(ast, Expr) && (ast.head === :error || ast.head === :incomplete)) ||
                        (pos > ncodeunits(input) && !endswith(input, '\n'))
                    # remaining text is incomplete (an error, or parser ran to the end but didn't stop with a newline):
                    # Insert all the remaining text as one line (might be empty)
                    tail = input[oldpos:end]
                    if !firstline
                        # strip leading whitespace, but only if it was the result of executing something
                        # (avoids modifying the user's current leading wip line)
                        tail = lstrip(tail)
                    end
                    if isprompt_paste # remove indentation spaces corresponding to the prompt
                        tail = replace(tail, r"^"m * ' '^jl_prompt_len => "")
                    end
                    LineEdit.replace_line(s, tail, true)
                    LineEdit.refresh_line(s)
                    break
                end
                # get the line and strip leading and trailing whitespace
                line = strip(input[oldpos:prevind(input, pos)])
                if !isempty(line)
                    if isprompt_paste # remove indentation spaces corresponding to the prompt
                        line = replace(line, r"^"m * ' '^jl_prompt_len => "")
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
        "^Q" => (s, o...) -> begin
            linfos = repl.last_shown_line_infos
            str = String(take!(LineEdit.buffer(s)))
            n = tryparse(Int, str)
            n === nothing && @goto writeback
            if n <= 0 || n > length(linfos) || startswith(linfos[n][1], "./REPL")
                @goto writeback
            end
            InteractiveUtils.edit(linfos[n][1], linfos[n][2])
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

    allprompts = [julia_prompt, shell_mode, help_mode, search_prompt, prefix_prompt]
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

# heuristic function to decide if the presence of a semicolon
# at the end of the expression was intended for suppressing output
function ends_with_semicolon(line::AbstractString)
    match = findlast(isequal(';'), line)
    if match !== nothing
        # state for comment parser, assuming that the `;` isn't in a string or comment
        # so input like ";#" will still thwart this to give the wrong (anti-conservative) answer
        comment = false
        comment_start = false
        comment_close = false
        comment_multi = 0
        for c in line[(match + 1):end]
            if comment_multi > 0
                # handle nested multi-line comments
                if comment_close && c == '#'
                    comment_close = false
                    comment_multi -= 1
                elseif comment_start && c == '='
                    comment_start = false
                    comment_multi += 1
                else
                    comment_start = (c == '#')
                    comment_close = (c == '=')
                end
            elseif comment
                # handle line comments
                if c == '\r' || c == '\n'
                    comment = false
                end
            elseif comment_start
                # see what kind of comment this is
                comment_start = false
                if c == '='
                    comment_multi = 1
                else
                    comment = true
                end
            elseif c == '#'
                # start handling for a comment
                comment_start = true
            else
                # outside of a comment, encountering anything but whitespace
                # means the semi-colon was internal to the expression
                isspace(c) || return false
            end
        end
        return true
    end
    return false
end

function run_frontend(repl::StreamREPL, backend::REPLBackendRef)
    have_color = hascolor(repl)
    Base.banner(repl.stream)
    d = REPLDisplay(repl)
    dopushdisplay = !in(d,Base.Multimedia.displays)
    dopushdisplay && pushdisplay(d)
    while !eof(repl.stream)
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

function start_repl_server(port::Int)
    return listen(port) do server, status
        client = accept(server)
        run_repl(client)
        nothing
    end
end

end # module
