# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Profile

Profiling support.

## CPU profiling
- `@profile foo()` to profile a specific call.
- `Profile.print()` to print the report. Paths are clickable links in supported terminals and specialized for JULIA_EDITOR etc.
- `Profile.clear()` to clear the buffer.
- Send a SIGUSR1 (on linux) or SIGINFO (on macOS/BSD) signal to the process to automatically trigger a profile and print. i.e. `kill -s SIGUSR1/SIGINFO 1234`, where 1234 is the pid of the julia process. On macOS & BSD platforms `ctrl-t` can be used directly.

## Memory profiling
- `Profile.Allocs.@profile [sample_rate=0.1] foo()` to sample allocations within a specific call. A sample rate of 1.0 will record everything; 0.0 will record nothing.
- `Profile.Allocs.print()` to print the report.
- `Profile.Allocs.clear()` to clear the buffer.

## Heap profiling
- `Profile.take_heap_snapshot()` to record a `.heapsnapshot` record of the heap.
- Set `JULIA_PROFILE_PEEK_HEAP_SNAPSHOT=true` to capture a heap snapshot when signal $(Sys.isbsd() ? "SIGINFO (ctrl-t)" : "SIGUSR1") is sent.
"""
module Profile

global print
export @profile, @profile_walltime
public clear,
    print,
    fetch,
    retrieve,
    add_fake_meta,
    flatten,
    callers,
    init,
    take_heap_snapshot,
    take_page_profile,
    clear_malloc_data,
    Allocs

import Base.StackTraces: lookup, UNKNOWN, show_spec_linfo, StackFrame
import Base: AnnotatedString
using StyledStrings: @styled_str

const nmeta = 4 # number of metadata fields per block (threadid, taskid, cpu_cycle_clock, thread_sleeping)

const slash = Sys.iswindows() ? "\\" : "/"

# deprecated functions: use `getdict` instead
lookup(ip::UInt) = lookup(convert(Ptr{Cvoid}, ip))

"""
    @profile

`@profile <expression>` runs your expression while taking periodic backtraces. These are
appended to an internal buffer of backtraces.
"""
macro profile(ex)
    return quote
        start_timer()
        Base.@__tryfinally(
            $(esc(ex))
            ,
            stop_timer()
        )
    end
end

"""
    @profile_walltime

`@profile_walltime <expression>` runs your expression while taking periodic backtraces of a sample of all live tasks (both running and not running).
These are appended to an internal buffer of backtraces.

It can be configured via `Profile.init`, same as the `Profile.@profile`, and that you can't use `@profile` simultaneously with `@profile_walltime`.

As mentioned above, since this tool sample not only running tasks, but also sleeping tasks and tasks performing IO,
it can be used to diagnose performance issues such as lock contention, IO bottlenecks, and other issues that are not visible in the CPU profile.
"""
macro profile_walltime(ex)
    return quote
        start_timer(true);
        Base.@__tryfinally(
            $(esc(ex))
            ,
            stop_timer()
        )
    end
end

# An internal function called to show the report after an information request (SIGINFO or SIGUSR1).
function _peek_report()
    iob = Base.AnnotatedIOBuffer()
    ioc = IOContext(IOContext(iob, stderr), :displaysize=>displaysize(stderr))
    print(ioc, groupby = [:thread, :task])
    Base.print(stderr, read(seekstart(iob), AnnotatedString))
end
# This is a ref so that it can be overridden by other profile info consumers.
const peek_report = Ref{Function}(_peek_report)

"""
    get_peek_duration()

Get the duration in seconds of the profile "peek" that is triggered via `SIGINFO` or `SIGUSR1`, depending on platform.
"""
get_peek_duration() = ccall(:jl_get_profile_peek_duration, Float64, ())
"""
    set_peek_duration(t::Float64)

Set the duration in seconds of the profile "peek" that is triggered via `SIGINFO` or `SIGUSR1`, depending on platform.
"""
set_peek_duration(t::Float64) = ccall(:jl_set_profile_peek_duration, Cvoid, (Float64,), t)



####
#### User-level functions
####

"""
    init(; n::Integer, delay::Real)

Configure the `delay` between backtraces (measured in seconds), and the number `n` of instruction pointers that may be
stored per thread. Each instruction pointer corresponds to a single line of code; backtraces generally consist of a long
list of instruction pointers. Note that 6 spaces for instruction pointers per backtrace are used to store metadata and two
NULL end markers. Current settings can be obtained by calling this function with no arguments, and each can be set independently
using keywords or in the order `(n, delay)`.
"""
function init(; n::Union{Nothing,Integer} = nothing, delay::Union{Nothing,Real} = nothing, limitwarn::Bool = true)
    n_cur = ccall(:jl_profile_maxlen_data, Csize_t, ())
    if n_cur == 0 && isnothing(n) && isnothing(delay)
        # indicates that the buffer hasn't been initialized at all, so set the default
        default_init()
        n_cur = ccall(:jl_profile_maxlen_data, Csize_t, ())
    end
    delay_cur = ccall(:jl_profile_delay_nsec, UInt64, ())/10^9
    if n === nothing && delay === nothing
        return n_cur, delay_cur
    end
    nnew = (n === nothing) ? n_cur : n
    delaynew = (delay === nothing) ? delay_cur : delay
    init(nnew, delaynew; limitwarn)
end

function init(n::Integer, delay::Real; limitwarn::Bool = true)
    sample_size_bytes = sizeof(Ptr) # == Sys.WORD_SIZE / 8
    buffer_samples = n
    buffer_size_bytes = buffer_samples * sample_size_bytes
    if buffer_size_bytes > 2^29 && Sys.WORD_SIZE == 32
        buffer_samples = floor(Int, 2^29 / sample_size_bytes)
        buffer_size_bytes = buffer_samples * sample_size_bytes
        limitwarn && @warn "Requested profile buffer limited to 512MB (n = $buffer_samples) given that this system is 32-bit"
    end
    status = ccall(:jl_profile_init, Cint, (Csize_t, UInt64), buffer_samples, round(UInt64, 10^9*delay))
    if status == -1
        error("could not allocate space for ", n, " instruction pointers ($(Base.format_bytes(buffer_size_bytes)))")
    end
end

function default_init()
    # init with default values
    # Use a max size of 10M profile samples, and fire timer every 1ms
    # (that should typically give around 100 seconds of record)
    @static if Sys.iswindows() && Sys.WORD_SIZE == 32
        # The Win32 unwinder is 1000x slower than elsewhere (around 1ms/frame),
        # so we don't want to slow the program down by quite that much
        n = 1_000_000
        delay = 0.01
    else
        # Keep these values synchronized with trigger_profile_peek
        n = 10_000_000
        delay = 0.001
    end
    init(n, delay, limitwarn = false)
end

# Checks whether the profile buffer has been initialized. If not, initializes it with the default size.
function check_init()
    buffer_size = @ccall jl_profile_maxlen_data()::Int
    if buffer_size == 0
        default_init()
    end
end

"""
    clear()

Clear any existing backtraces from the internal buffer.
"""
clear() = ccall(:jl_profile_clear_data, Cvoid, ())

const LineInfoDict = Dict{UInt64, Vector{StackFrame}}
const LineInfoFlatDict = Dict{UInt64, StackFrame}

struct ProfileFormat
    maxdepth::Int
    mincount::Int
    noisefloor::Float64
    sortedby::Symbol
    combine::Bool
    C::Bool
    recur::Symbol
    function ProfileFormat(;
        C = false,
        combine = true,
        maxdepth::Int = typemax(Int),
        mincount::Int = 0,
        noisefloor = 0,
        sortedby::Symbol = :filefuncline,
        recur::Symbol = :off)
        return new(maxdepth, mincount, noisefloor, sortedby, combine, C, recur)
    end
end

# offsets of the metadata in the data stream
const META_OFFSET_SLEEPSTATE = 2
const META_OFFSET_CPUCYCLECLOCK = 3
const META_OFFSET_TASKID = 4
const META_OFFSET_THREADID = 5

"""
    print([io::IO = stdout,] [data::Vector = fetch()], [lidict::Union{LineInfoDict, LineInfoFlatDict} = getdict(data)]; kwargs...)
    print(path::String, [cols::Int = 1000], [data::Vector = fetch()], [lidict::Union{LineInfoDict, LineInfoFlatDict} = getdict(data)]; kwargs...)

Prints profiling results to `io` (by default, `stdout`). If you do not
supply a `data` vector, the internal buffer of accumulated backtraces
will be used. Paths are clickable links in supported terminals and
specialized for [`JULIA_EDITOR`](@ref) with line numbers, or just file
links if no editor is set.

The keyword arguments can be any combination of:

 - `format` -- Determines whether backtraces are printed with (default, `:tree`) or without (`:flat`)
   indentation indicating tree structure.

 - `C` -- If `true`, backtraces from C and Fortran code are shown (normally they are excluded).

 - `combine` -- If `true` (default), instruction pointers are merged that correspond to the same line of code.

 - `maxdepth` -- Limits the depth higher than `maxdepth` in the `:tree` format.

 - `sortedby` -- Controls the order in `:flat` format. `:filefuncline` (default) sorts by the source
    line, `:count` sorts in order of number of collected samples, and `:overhead` sorts by the number of samples
    incurred by each function by itself.

 - `groupby` -- Controls grouping over tasks and threads, or no grouping. Options are `:none` (default), `:thread`, `:task`,
    `[:thread, :task]`, or `[:task, :thread]` where the last two provide nested grouping.

 - `noisefloor` -- Limits frames that exceed the heuristic noise floor of the sample (only applies to format `:tree`).
    A suggested value to try for this is 2.0 (the default is 0). This parameter hides samples for which `n <= noisefloor * √N`,
    where `n` is the number of samples on this line, and `N` is the number of samples for the callee.

 - `mincount` -- Limits the printout to only those lines with at least `mincount` occurrences.

 - `recur` -- Controls the recursion handling in `:tree` format. `:off` (default) prints the tree as normal. `:flat` instead
    compresses any recursion (by ip), showing the approximate effect of converting any self-recursion into an iterator.
    `:flatc` does the same but also includes collapsing of C frames (may do odd things around `jl_apply`).

 - `threads::Union{Int,AbstractVector{Int}}` -- Specify which threads to include snapshots from in the report. Note that
    this does not control which threads samples are collected on (which may also have been collected on another machine).

 - `tasks::Union{Int,AbstractVector{Int}}` -- Specify which tasks to include snapshots from in the report. Note that this
    does not control which tasks samples are collected within.

!!! compat "Julia 1.8"
    The `groupby`, `threads`, and `tasks` keyword arguments were introduced in Julia 1.8.

!!! note
    Profiling on windows is limited to the main thread. Other threads have not been sampled and will not show in the report.

"""
function print(io::IO,
        data::Vector{<:Unsigned} = fetch(),
        lidict::Union{LineInfoDict, LineInfoFlatDict} = getdict(data)
        ;
        format = :tree,
        C = false,
        combine = true,
        maxdepth::Int = typemax(Int),
        mincount::Int = 0,
        noisefloor = 0,
        sortedby::Symbol = :filefuncline,
        groupby::Union{Symbol,AbstractVector{Symbol}} = :none,
        recur::Symbol = :off,
        threads::Union{Int,AbstractVector{Int}} = 1:typemax(Int),
        tasks::Union{UInt,AbstractVector{UInt}} = typemin(UInt):typemax(UInt))

    pf = ProfileFormat(;C, combine, maxdepth, mincount, noisefloor, sortedby, recur)
    if groupby === :none
        print_group(io, data, lidict, pf, format, threads, tasks, false)
    else
        if !in(groupby, [:thread, :task, [:task, :thread], [:thread, :task]])
            error(ArgumentError("Unrecognized groupby option: $groupby. Options are :none (default), :task, :thread, [:task, :thread], or [:thread, :task]"))
        elseif Sys.iswindows() && in(groupby, [:thread, [:task, :thread], [:thread, :task]])
            @warn "Profiling on windows is limited to the main thread. Other threads have not been sampled and will not show in the report"
        end
        any_nosamples = true
        if format === :tree
            Base.print(io, "Overhead ╎ [+additional indent] Count File:Line  Function\n")
            Base.print(io, "=========================================================\n")
        end
        if groupby == [:task, :thread]
            taskids = intersect(get_task_ids(data), tasks)
            isempty(taskids) && (any_nosamples = true)
            for taskid in taskids
                threadids = intersect(get_thread_ids(data, taskid), threads)
                if length(threadids) == 0
                    any_nosamples = true
                else
                    nl = length(threadids) > 1 ? "\n" : ""
                    printstyled(io, "Task $(Base.repr(taskid))$nl"; bold=true, color=Base.debug_color())
                    for threadid in threadids
                        printstyled(io, " Thread $threadid ($(Threads.threadpooldescription(threadid))) "; bold=true, color=Base.info_color())
                        nosamples = print_group(io, data, lidict, pf, format, threadid, taskid, true)
                        nosamples && (any_nosamples = true)
                        println(io)
                    end
                end
            end
        elseif groupby == [:thread, :task]
            threadids = intersect(get_thread_ids(data), threads)
            isempty(threadids) && (any_nosamples = true)
            for threadid in threadids
                taskids = intersect(get_task_ids(data, threadid), tasks)
                if length(taskids) == 0
                    any_nosamples = true
                else
                    nl = length(taskids) > 1 ? "\n" : ""
                    printstyled(io, "Thread $threadid ($(Threads.threadpooldescription(threadid)))$nl"; bold=true, color=Base.info_color())
                    for taskid in taskids
                        printstyled(io, " Task $(Base.repr(taskid)) "; bold=true, color=Base.debug_color())
                        nosamples = print_group(io, data, lidict, pf, format, threadid, taskid, true)
                        nosamples && (any_nosamples = true)
                        println(io)
                    end
                end
            end
        elseif groupby === :task
            threads = 1:typemax(Int)
            taskids = intersect(get_task_ids(data), tasks)
            isempty(taskids) && (any_nosamples = true)
            for taskid in taskids
                printstyled(io, "Task $(Base.repr(taskid)) "; bold=true, color=Base.debug_color())
                nosamples = print_group(io, data, lidict, pf, format, threads, taskid, true)
                nosamples && (any_nosamples = true)
                println(io)
            end
        elseif groupby === :thread
            tasks = 1:typemax(UInt)
            threadids = intersect(get_thread_ids(data), threads)
            isempty(threadids) && (any_nosamples = true)
            for threadid in threadids
                printstyled(io, "Thread $threadid ($(Threads.threadpooldescription(threadid))) "; bold=true, color=Base.info_color())
                nosamples = print_group(io, data, lidict, pf, format, threadid, tasks, true)
                nosamples && (any_nosamples = true)
                println(io)
            end
        end
        any_nosamples && warning_empty(summary = true)
    end
    return
end

function print(path::String, cols::Int = 1000, args...; kwargs...)
    open(path, "w") do io
        ioc = IOContext(io, :displaysize=>(1000,cols))
        print(ioc, args...; kwargs...)
    end
end

"""
    print([io::IO = stdout,] data::Vector, lidict::LineInfoDict; kwargs...)

Prints profiling results to `io`. This variant is used to examine results exported by a
previous call to [`retrieve`](@ref). Supply the vector `data` of backtraces and
a dictionary `lidict` of line information.

See `Profile.print([io], data)` for an explanation of the valid keyword arguments.
"""
print(data::Vector{<:Unsigned} = fetch(), lidict::Union{LineInfoDict, LineInfoFlatDict} = getdict(data); kwargs...) =
    print(stdout, data, lidict; kwargs...)

function print_group(io::IO, data::Vector{<:Unsigned}, lidict::Union{LineInfoDict, LineInfoFlatDict}, fmt::ProfileFormat,
                format::Symbol, threads::Union{Int,AbstractVector{Int}}, tasks::Union{UInt,AbstractVector{UInt}},
                is_subsection::Bool = false)
    cols::Int = Base.displaysize(io)[2]
    data = convert(Vector{UInt64}, data)
    fmt.recur ∈ (:off, :flat, :flatc) || throw(ArgumentError("recur value not recognized"))
    if format === :tree
        nosamples = tree(io, data, lidict, cols, fmt, threads, tasks, is_subsection)
        return nosamples
    elseif format === :flat
        fmt.recur === :off || throw(ArgumentError("format flat only implements recur=:off"))
        nosamples = flat(io, data, lidict, cols, fmt, threads, tasks, is_subsection)
        return nosamples
    else
        throw(ArgumentError("output format $(repr(format)) not recognized"))
    end
end

function get_task_ids(data::Vector{<:Unsigned}, threadid = nothing)
    taskids = UInt[]
    for i in length(data):-1:1
        if is_block_end(data, i)
            if isnothing(threadid) || data[i - META_OFFSET_THREADID] == threadid
                taskid = data[i - META_OFFSET_TASKID]
                !in(taskid, taskids) && push!(taskids, taskid)
            end
        end
    end
    return taskids
end

function get_thread_ids(data::Vector{<:Unsigned}, taskid = nothing)
    threadids = Int[]
    for i in length(data):-1:1
        if is_block_end(data, i)
            if isnothing(taskid) || data[i - META_OFFSET_TASKID] == taskid
                threadid = data[i - META_OFFSET_THREADID]
                !in(threadid, threadids) && push!(threadids, threadid)
            end
        end
    end
    return sort(threadids)
end

function is_block_end(data, i)
    i < nmeta + 1 && return false
    # 32-bit linux has been seen to have rogue NULL ips, so we use two to
    # indicate block end, where the 2nd is the actual end index.
    # and we could have (though very unlikely):
    # 1:<stack><metadata><null><null><NULL><metadata><null><null>:end
    # and we want to ignore the triple NULL (which is an ip).
    return data[i] == 0 && data[i - 1] == 0 && data[i - META_OFFSET_SLEEPSTATE] != 0
end

function has_meta(data)
    for i in 6:length(data)
        data[i] == 0 || continue                            # first block end null
        data[i - 1] == 0 || continue                        # second block end null
        data[i - META_OFFSET_SLEEPSTATE] in 1:3 || continue # 1 for not sleeping, 2 for sleeping, 3 for task profiler fake state
                                                            # See definition in `src/julia_internal.h`
        data[i - META_OFFSET_CPUCYCLECLOCK] != 0 || continue
        data[i - META_OFFSET_TASKID] != 0 || continue
        data[i - META_OFFSET_THREADID] != 0 || continue
        return true
    end
    return false
end

"""
    retrieve(; kwargs...) -> data, lidict

"Exports" profiling results in a portable format, returning the set of all backtraces
(`data`) and a dictionary that maps the (session-specific) instruction pointers in `data` to
`LineInfo` values that store the file name, function name, and line number. This function
allows you to save profiling results for future analysis.
"""
function retrieve(; kwargs...)
    data = fetch(; kwargs...)
    return (data, getdict(data))
end

function getdict(data::Vector{UInt})
    dict = LineInfoDict()
    return getdict!(dict, data)
end

function getdict!(dict::LineInfoDict, data::Vector{UInt})
    # we don't want metadata here as we're just looking up ips
    unique_ips = unique(has_meta(data) ? strip_meta(data) : data)
    n_unique_ips = length(unique_ips)
    n_unique_ips == 0 && return dict
    iplookups = similar(unique_ips, Vector{StackFrame})
    sort!(unique_ips) # help each thread to get a disjoint set of libraries, as much if possible
    @sync for indexes_part in Iterators.partition(eachindex(unique_ips), div(n_unique_ips, Threads.threadpoolsize(), RoundUp))
        Threads.@spawn begin
            for i in indexes_part
                iplookups[i] = _lookup_corrected(unique_ips[i])
            end
        end
    end
    for i in eachindex(unique_ips)
        dict[unique_ips[i]] = iplookups[i]
    end
    return dict
end

function _lookup_corrected(ip::UInt)
    st = lookup(convert(Ptr{Cvoid}, ip))
    # To correct line numbers for moving code, put it in the form expected by
    # Base.update_stackframes_callback[]
    stn = map(x->(x, 1), st)
    # Note: Base.update_stackframes_callback[] should be data-race free
    try Base.invokelatest(Base.update_stackframes_callback[], stn) catch end
    return map(first, stn)
end

"""
    flatten(btdata::Vector, lidict::LineInfoDict) -> (newdata::Vector{UInt64}, newdict::LineInfoFlatDict)

Produces "flattened" backtrace data. Individual instruction pointers
sometimes correspond to a multi-frame backtrace due to inlining; in
such cases, this function inserts fake instruction pointers for the
inlined calls, and returns a dictionary that is a 1-to-1 mapping
between instruction pointers and a single StackFrame.
"""
function flatten(data::Vector, lidict::LineInfoDict)
    # Makes fake instruction pointers, counting down from typemax(UInt)
    newip = typemax(UInt64) - 1
    taken = Set(keys(lidict))  # make sure we don't pick one that's already used
    newdict = Dict{UInt64,StackFrame}()
    newmap  = Dict{UInt64,Vector{UInt64}}()
    for (ip, trace) in lidict
        if length(trace) == 1
            newdict[ip] = trace[1]
        else
            newm = UInt64[]
            for sf in trace
                while newip ∈ taken && newip > 0
                    newip -= 1
                end
                newip == 0 && error("all possible instruction pointers used")
                push!(newm, newip)
                newdict[newip] = sf
                newip -= 1
            end
            newmap[ip] = newm
        end
    end
    newdata = UInt64[]
    for ip::UInt64 in data
        if haskey(newmap, ip)
            append!(newdata, newmap[ip])
        else
            push!(newdata, ip)
        end
    end
    return (newdata, newdict)
end

const SRC_DIR = normpath(joinpath(Sys.BUILD_ROOT_PATH, "src"))
const COMPILER_DIR = "../usr/share/julia/Compiler/"

# Take a file-system path and try to form a concise representation of it
# based on the package ecosystem
# filenamecache is a dict of spath -> (fullpath or "" if !isfile, modulename, shortpath)
function short_path(spath::Symbol, filenamecache::Dict{Symbol, Tuple{String,String,String}})
    return get!(filenamecache, spath) do
        path = Base.fixup_stdlib_path(string(spath))
        path_norm = normpath(path)
        possible_base_path = normpath(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "base", path))
        lib_dir = abspath(Sys.BINDIR, Base.LIBDIR)
        if startswith(path_norm, SRC_DIR)
            remainder = only(split(path_norm, SRC_DIR, keepempty=false))
            return (isfile(path_norm) ? path_norm : ""), "@juliasrc", remainder
        elseif startswith(path_norm, lib_dir)
            remainder = only(split(path_norm, lib_dir, keepempty=false))
            return (isfile(path_norm) ? path_norm : ""), "@julialib", remainder
        elseif contains(path, COMPILER_DIR)
            remainder = split(path, COMPILER_DIR, keepempty=false)[end]
            possible_compiler_path = normpath(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "Compiler", remainder))
            return (isfile(possible_compiler_path) ? possible_compiler_path : ""), "@Compiler", remainder
        elseif isabspath(path)
            if ispath(path)
                # try to replace the file-system prefix with a short "@Module" one,
                # assuming that profile came from the current machine
                # (or at least has the same file-system layout)
                root = path
                while !isempty(root)
                    root, base = splitdir(root)
                    isempty(base) && break
                    @assert startswith(path, root)
                    for proj in Base.project_names
                        project_file = joinpath(root, proj)
                        if Base.isfile_casesensitive(project_file)
                            pkgid = Base.project_file_name_uuid(project_file, "")
                            isempty(pkgid.name) && return path, "", path # bad Project file
                            # return the joined the module name prefix and path suffix
                            _short_path = path[nextind(path, sizeof(root)):end]
                            return path, string("@", pkgid.name), _short_path
                        end
                    end
                end
            end
            return path, "", path
        elseif isfile(possible_base_path)
            # do the same mechanic for Base (or Core/Compiler) files as above,
            # but they start from a relative path
            return possible_base_path, "@Base", normpath(path)
        else
            # for non-existent relative paths (such as "REPL[1]"), just consider simplifying them
            path = normpath(path)
            return "", "", path # drop leading "./"
        end
    end
end

"""
    callers(funcname, [data, lidict], [filename=<filename>], [linerange=<start:stop>])::Vector{Tuple{count, lineinfo}}

Given a previous profiling run, determine who called a particular function. Supplying the
filename (and optionally, range of line numbers over which the function is defined) allows
you to disambiguate an overloaded method. The returned value is a vector containing a count
of the number of calls and line information about the caller. One can optionally supply
backtrace `data` obtained from [`retrieve`](@ref); otherwise, the current internal
profile buffer is used.
"""
function callers end

function callers(funcname::String, bt::Vector, lidict::LineInfoFlatDict; filename = nothing, linerange = nothing)
    if filename === nothing && linerange === nothing
        return callersf(li -> String(li.func) == funcname,
            bt, lidict)
    end
    filename === nothing && throw(ArgumentError("if supplying linerange, you must also supply the filename"))
    filename = String(filename)
    if linerange === nothing
        return callersf(li -> String(li.func) == funcname && String(li.file) == filename,
            bt, lidict)
    else
        return callersf(li -> String(li.func) == funcname && String(li.file) == filename && in(li.line, linerange),
            bt, lidict)
    end
end

callers(funcname::String, bt::Vector, lidict::LineInfoDict; kwargs...) =
    callers(funcname, flatten(bt, lidict)...; kwargs...)
callers(funcname::String; kwargs...) = callers(funcname, retrieve()...; kwargs...)
callers(func::Function, bt::Vector, lidict::LineInfoFlatDict; kwargs...) =
    callers(string(func), bt, lidict; kwargs...)
callers(func::Function; kwargs...) = callers(string(func), retrieve()...; kwargs...)

##
## For --track-allocation
##
# Reset the malloc log. Used to avoid counting memory allocated during
# compilation.

"""
    clear_malloc_data()

Clears any stored memory allocation data when running julia with `--track-allocation`.
Execute the command(s) you want to test (to force JIT-compilation), then call
[`clear_malloc_data`](@ref). Then execute your command(s) again, quit
Julia, and examine the resulting `*.mem` files.
"""
clear_malloc_data() = ccall(:jl_clear_malloc_data, Cvoid, ())

# C wrappers
function start_timer(all_tasks::Bool=false)
    check_init() # if the profile buffer hasn't been initialized, initialize with default size
    status = ccall(:jl_profile_start_timer, Cint, (Bool,), all_tasks)
    if status < 0
        error(error_codes[status])
    end
end


stop_timer() = ccall(:jl_profile_stop_timer, Cvoid, ())

is_running() = ccall(:jl_profile_is_running, Cint, ())!=0

is_buffer_full() = ccall(:jl_profile_is_buffer_full, Cint, ())!=0

get_data_pointer() = convert(Ptr{UInt}, ccall(:jl_profile_get_data, Ptr{UInt8}, ()))

len_data() = convert(Int, ccall(:jl_profile_len_data, Csize_t, ()))

maxlen_data() = convert(Int, ccall(:jl_profile_maxlen_data, Csize_t, ()))

error_codes = Dict(
    -1=>"cannot specify signal action for profiling",
    -2=>"cannot create the timer for profiling",
    -3=>"cannot start the timer for profiling",
    -4=>"cannot unblock SIGUSR1")


"""
    fetch(;include_meta = true) -> data

Return a copy of the buffer of profile backtraces. Note that the
values in `data` have meaning only on this machine in the current session, because it
depends on the exact memory addresses used in JIT-compiling. This function is primarily for
internal use; [`retrieve`](@ref) may be a better choice for most users.
By default metadata such as threadid and taskid is included. Set `include_meta` to `false` to strip metadata.
"""
function fetch(;include_meta = true, limitwarn = true)
    maxlen = maxlen_data()
    if maxlen == 0
        error("The profiling data buffer is not initialized. A profile has not been requested this session.")
    end
    len = len_data()
    if limitwarn && is_buffer_full()
        @warn """The profile data buffer is full; profiling probably terminated
                 before your program finished. To profile for longer runs, call
                 `Profile.init()` with a larger buffer and/or larger delay."""
    end
    data = Vector{UInt}(undef, len)
    GC.@preserve data unsafe_copyto!(pointer(data), get_data_pointer(), len)
    if include_meta || isempty(data)
        return data
    end
    return strip_meta(data)
end

function strip_meta(data)
    nblocks = count(Base.Fix1(is_block_end, data), eachindex(data))
    data_stripped = Vector{UInt}(undef, length(data) - (nblocks * (nmeta + 1)))
    j = length(data_stripped)
    i = length(data)
    while i > 0 && j > 0
        data_stripped[j] = data[i]
        if is_block_end(data, i)
            i -= (nmeta + 1) # metadata fields and the extra NULL IP
        end
        i -= 1
        j -= 1
    end
    @assert i == j == 0 "metadata stripping failed"
    return data_stripped
end

"""
    Profile.add_fake_meta(data; threadid = 1, taskid = 0xf0f0f0f0) -> data_with_meta

The converse of `Profile.fetch(;include_meta = false)`; this will add fake metadata, and can be used
for compatibility and by packages (e.g., FlameGraphs.jl) that would rather not depend on the internal
details of the metadata format.
"""
function add_fake_meta(data; threadid = 1, taskid = 0xf0f0f0f0)
    threadid == 0 && error("Fake threadid cannot be 0")
    taskid == 0 && error("Fake taskid cannot be 0")
    !isempty(data) && has_meta(data) && error("input already has metadata")
    cpu_clock_cycle = UInt64(99)
    data_with_meta = similar(data, 0)
    for i in eachindex(data)
        val = data[i]
        if iszero(val)
            # META_OFFSET_THREADID, META_OFFSET_TASKID, META_OFFSET_CPUCYCLECLOCK, META_OFFSET_SLEEPSTATE
            push!(data_with_meta, threadid, taskid, cpu_clock_cycle+=1, false+1, 0, 0)
        else
            push!(data_with_meta, val)
        end
    end
    return data_with_meta
end

## Print as a flat list
# Counts the number of times each line appears, at any nesting level and at the topmost level
# Merging multiple equivalent entries and recursive calls
function parse_flat(::Type{T}, data::Vector{UInt64}, lidict::Union{LineInfoDict, LineInfoFlatDict}, C::Bool,
                    threads::Union{Int,AbstractVector{Int}}, tasks::Union{UInt,AbstractVector{UInt}}) where {T}
    !isempty(data) && !has_meta(data) && error("Profile data is missing required metadata")
    lilist = StackFrame[]
    n = Int[]
    m = Int[]
    lilist_idx = Dict{T, Int}()
    recursive = Set{T}()
    leaf = 0
    totalshots = 0
    startframe = length(data)
    skip = false
    nsleeping = 0
    is_task_profile = false
    for i in startframe:-1:1
        (startframe - 1) >= i >= (startframe - (nmeta + 1)) && continue # skip metadata (its read ahead below) and extra block end NULL IP
        ip = data[i]
        if is_block_end(data, i)
            # read metadata
            thread_sleeping_state = data[i - META_OFFSET_SLEEPSTATE] - 1 # subtract 1 as state is incremented to avoid being equal to 0
            if thread_sleeping_state == 2
                is_task_profile = true
            end
            # cpu_cycle_clock = data[i - META_OFFSET_CPUCYCLECLOCK]
            taskid = data[i - META_OFFSET_TASKID]
            threadid = data[i - META_OFFSET_THREADID]
            if !in(threadid, threads) || !in(taskid, tasks)
                skip = true
                continue
            end
            if thread_sleeping_state == 1
                nsleeping += 1
            end
            skip = false
            totalshots += 1
            empty!(recursive)
            if leaf != 0
                m[leaf] += 1
            end
            leaf = 0
            startframe = i
        elseif !skip
            frames = lidict[ip]
            nframes = (frames isa Vector ? length(frames) : 1)
            # the last lookup is the non-inlined root frame, the first is the inlined leaf frame
            for j = nframes:-1:1
                frame = (frames isa Vector ? frames[j] : frames)
                !C && frame.from_c && continue
                key = (T === UInt64 ? ip : frame)
                idx = get!(lilist_idx, key, length(lilist) + 1)
                if idx > length(lilist)
                    push!(recursive, key)
                    push!(lilist, frame)
                    push!(n, 1)
                    push!(m, 0)
                elseif !(key in recursive)
                    push!(recursive, key)
                    n[idx] += 1
                end
                leaf = idx
            end
        end
    end
    @assert length(lilist) == length(n) == length(m) == length(lilist_idx)
    return (lilist, n, m, totalshots, nsleeping, is_task_profile)
end

const FileNameMap = Dict{Symbol,Tuple{String,String,String}}

function flat(io::IO, data::Vector{UInt64}, lidict::Union{LineInfoDict, LineInfoFlatDict}, cols::Int, fmt::ProfileFormat,
                threads::Union{Int,AbstractVector{Int}}, tasks::Union{UInt,AbstractVector{UInt}}, is_subsection::Bool)
    lilist, n, m, totalshots, nsleeping, is_task_profile = parse_flat(fmt.combine ? StackFrame : UInt64, data, lidict, fmt.C, threads, tasks)
    if false # optional: drop the "non-interpretable" ones
        keep = map(frame -> frame != UNKNOWN && frame.line != 0, lilist)
        lilist = lilist[keep]
        n = n[keep]
        m = m[keep]
    end
    util_perc = (1 - (nsleeping / totalshots)) * 100
    filenamemap = FileNameMap()
    if isempty(lilist)
        if is_subsection
            Base.print(io, "Total snapshots: ")
            printstyled(io, "$(totalshots)", color=Base.warn_color())
            Base.print(io, ". Utilization: ", round(Int, util_perc), "%\n")
        else
            warning_empty()
        end
        return true
    end
    is_subsection || print_flat(io, lilist, n, m, cols, filenamemap, fmt)
    if is_task_profile
        Base.print(io, "Total snapshots: ", totalshots, "\n")
    else
        Base.print(io, "Total snapshots: ", totalshots, ". Utilization: ", round(Int, util_perc), "%")
    end
    if is_subsection
        println(io)
        print_flat(io, lilist, n, m, cols, filenamemap, fmt)
    elseif !is_task_profile
        Base.print(io, " across all threads and tasks. Use the `groupby` kwarg to break down by thread and/or task.\n")
    end
    return false
end

# make a terminal-clickable link to the file and linenum.
# Similar to `define_default_editors` in `Base.Filesystem` but for creating URIs not commands
function editor_link(path::String, linenum::Int)
    # Note: the editor path can include spaces (if escaped) and flags.
    editor = nothing
    for var in ["JULIA_EDITOR", "VISUAL", "EDITOR"]
        str = get(ENV, var, nothing)
        str isa String || continue
        editor = str
        break
    end
    path_encoded = Base.Filesystem.encode_uri_component(path)
    if editor !== nothing
        if editor == "code"
            return "vscode://file/$path_encoded:$linenum"
        elseif editor == "subl" || editor == "sublime_text"
            return "subl://open?url=file://$path_encoded&line=$linenum"
        elseif editor == "idea" || occursin("idea", editor)
            return "idea://open?file=$path_encoded&line=$linenum"
        elseif editor == "pycharm"
            return "pycharm://open?file=$path_encoded&line=$linenum"
        elseif editor == "atom"
            return "atom://core/open/file?filename=$path_encoded&line=$linenum"
        elseif editor == "emacsclient" || editor == "emacs"
            return "emacs://open?file=$path_encoded&line=$linenum"
        elseif editor == "vim" || editor == "nvim"
            # Note: Vim/Nvim may not support standard URI schemes without specific plugins
            return "vim://open?file=$path_encoded&line=$linenum"
        end
    end
    # fallback to generic URI, but line numbers are not supported by generic URI
    return Base.Filesystem.uripath(path)
end

function print_flat(io::IO, lilist::Vector{StackFrame},
        n::Vector{Int}, m::Vector{Int},
        cols::Int, filenamemap::FileNameMap,
        fmt::ProfileFormat)
    if fmt.sortedby === :count
        p = sortperm(n)
    elseif fmt.sortedby === :overhead
        p = sortperm(m)
    else
        p = liperm(lilist)
    end
    lilist = lilist[p]
    n = n[p]
    m = m[p]
    pkgnames_filenames = Tuple{String,String,String}[short_path(li.file, filenamemap) for li in lilist]
    funcnames = String[string(li.func) for li in lilist]
    wcounts = max(6, ndigits(maximum(n)))
    wself = max(9, ndigits(maximum(m)))
    maxline = 1
    maxfile = 6
    maxfunc = 10
    for i in eachindex(lilist)
        li = lilist[i]
        maxline = max(maxline, li.line)
        maxfunc = max(maxfunc, textwidth(funcnames[i]))
        maxfile = max(maxfile, sum(textwidth, pkgnames_filenames[i][2:3]) + 1)
    end
    wline = max(5, ndigits(maxline))
    ntext = max(20, cols - wcounts - wself - wline - 3)
    maxfunc += 25 # for type signatures
    if maxfile + maxfunc <= ntext
        wfile = maxfile
        wfunc = ntext - maxfunc # take the full width (for type sig)
    else
        wfile = 2*ntext÷5
        wfunc = 3*ntext÷5
    end
    println(io, lpad("Count", wcounts, " "), " ", lpad("Overhead", wself, " "), " ",
            rpad("File", wfile, " "), " ", lpad("Line", wline, " "), " Function")
    println(io, lpad("=====", wcounts, " "), " ", lpad("========", wself, " "), " ",
            rpad("====", wfile, " "), " ", lpad("====", wline, " "), " ========")
    for i in eachindex(n)
        n[i] < fmt.mincount && continue
        li = lilist[i]
        Base.print(io, lpad(string(n[i]), wcounts, " "), " ")
        Base.print(io, lpad(string(m[i]), wself, " "), " ")
        if li == UNKNOWN
            if !fmt.combine && li.pointer != 0
                Base.print(io, "@0x", string(li.pointer, base=16))
            else
                Base.print(io, "[any unknown stackframes]")
            end
        else
            path, pkgname, file = pkgnames_filenames[i]
            isempty(file) && (file = "[unknown file]")
            pkgcolor = get!(() -> popfirst!(Base.STACKTRACE_MODULECOLORS), PACKAGE_FIXEDCOLORS, pkgname)
            Base.printstyled(io, pkgname, color=pkgcolor)
            file_trunc = ltruncate(file, max(1, wfile))
            wpad = wfile - textwidth(pkgname)
            if !isempty(pkgname) && !startswith(file_trunc, slash)
                Base.print(io, slash)
                wpad -= 1
            end
            if isempty(path)
                Base.print(io, rpad(file_trunc, wpad, " "))
            else
                link = editor_link(path, li.line)
                Base.print(io, rpad(styled"{link=$link:$file_trunc}", wpad, " "))
            end
            Base.print(io, lpad(li.line > 0 ? string(li.line) : "?", wline, " "), " ")
            fname = funcnames[i]
            if !li.from_c && li.linfo !== nothing
                fname = sprint(show_spec_linfo, li)
            end
            isempty(fname) && (fname = "[unknown function]")
            Base.print(io, rtruncate(fname, wfunc))
        end
        println(io)
    end
    nothing
end

## A tree representation

# Representation of a prefix trie of backtrace counts
mutable struct StackFrameTree{T} # where T <: Union{UInt64, StackFrame}
    # content fields:
    frame::StackFrame
    count::Int          # number of frames this appeared in
    overhead::Int       # number frames where this was the code being executed
    flat_count::Int     # number of times this frame was in the flattened representation (unlike count, this'll sum to 100% of parent)
    max_recur::Int      # maximum number of times this frame was the *top* of the recursion in the stack
    count_recur::Int    # sum of the number of times this frame was the *top* of the recursion in a stack (divide by count to get an average)
    sleeping::Bool      # whether this frame was in a sleeping state
    down::Dict{T, StackFrameTree{T}}
    # construction workers:
    recur::Int
    builder_key::Vector{UInt64}
    builder_value::Vector{StackFrameTree{T}}
    up::StackFrameTree{T}
    StackFrameTree{T}() where {T} = new(UNKNOWN, 0, 0, 0, 0, 0, true, Dict{T, StackFrameTree{T}}(), 0, UInt64[], StackFrameTree{T}[])
end


const indent_s = "    ╎"^10
const indent_z = collect(eachindex(indent_s))
function indent(depth::Int)
    depth < 1 && return ""
    depth <= length(indent_z) && return indent_s[1:indent_z[depth]]
    div, rem = divrem(depth, length(indent_z))
    indent = indent_s^div
    rem != 0 && (indent *= SubString(indent_s, 1, indent_z[rem]))
    return indent
end

# mimics Stacktraces
const PACKAGE_FIXEDCOLORS = Dict{String, Any}("@Base" => :gray, "@Core" => :gray)

function tree_format(frames::Vector{<:StackFrameTree}, level::Int, cols::Int, maxes, filenamemap::FileNameMap, showpointer::Bool)
    nindent = min(cols>>1, level)
    ndigoverhead = ndigits(maxes.overhead)
    ndigcounts = ndigits(maxes.count)
    ndigline = ndigits(maximum(frame.frame.line for frame in frames)) + 6
    ntext = max(30, cols - ndigoverhead - nindent - ndigcounts - ndigline - 6)
    widthfile = 2*ntext÷5 # min 12
    strs = Vector{AnnotatedString{String}}(undef, length(frames))
    showextra = false
    if level > nindent
        nextra = level - nindent
        nindent -= ndigits(nextra) + 2
        showextra = true
    end
    for i in eachindex(frames)
        frame = frames[i]
        li = frame.frame
        stroverhead = lpad(frame.overhead > 0 ? string(frame.overhead) : "", ndigoverhead, " ")
        base = nindent == 0 ? "" : indent(nindent - 1) * " "
        if showextra
            base = string(base, "+", nextra, " ")
        end
        strcount = rpad(string(frame.count), ndigcounts, " ")
        if frame.sleeping
            stroverhead = styled"{gray:$(stroverhead)}"
            strcount = styled"{gray:$(strcount)}"
        end
        if li != UNKNOWN
            if li.line == li.pointer
                strs[i] = string(stroverhead, "╎", base, strcount, " ",
                    "[unknown function] (pointer: 0x",
                    string(li.pointer, base = 16, pad = 2*sizeof(Ptr{Cvoid})),
                    ")")
            else
                if !li.from_c && li.linfo !== nothing
                    fname = sprint(show_spec_linfo, li)
                else
                    fname = string(li.func)
                end
                frame.sleeping && (fname = styled"{gray:$(fname)}")
                path, pkgname, filename = short_path(li.file, filenamemap)
                if showpointer
                    fname = string(
                        "0x",
                        string(li.pointer, base = 16, pad = 2*sizeof(Ptr{Cvoid})),
                        " ",
                        fname)
                end
                pkgcolor = get!(() -> popfirst!(Base.STACKTRACE_MODULECOLORS), PACKAGE_FIXEDCOLORS, pkgname)
                remaining_path = ltruncate(filename, max(1, widthfile - textwidth(pkgname) - 1))
                linenum = li.line == -1 ? "?" : string(li.line)
                _slash = (!isempty(pkgname) && !startswith(remaining_path, slash)) ? slash : ""
                styled_path = styled"{$pkgcolor:$pkgname}$(_slash)$remaining_path:$linenum"
                rich_file = if isempty(path)
                    styled_path
                else
                    link = editor_link(path, li.line)
                    styled"{link=$link:$styled_path}"
                end
                strs[i] = Base.annotatedstring(stroverhead, "╎", base, strcount, " ", rich_file, "  ", fname)
                if frame.overhead > 0
                    strs[i] = styled"{bold:$(strs[i])}"
                end
            end
        else
            strs[i] = string(stroverhead, "╎", base, strcount, " [unknown stackframe]")
        end
        strs[i] = rtruncate(strs[i], cols)
    end
    return strs
end

# turn a list of backtraces into a tree (implicitly separated by NULL markers)
function tree!(root::StackFrameTree{T}, all::Vector{UInt64}, lidict::Union{LineInfoFlatDict, LineInfoDict}, C::Bool, recur::Symbol,
                threads::Union{Int,AbstractVector{Int},Nothing}=nothing, tasks::Union{UInt,AbstractVector{UInt},Nothing}=nothing) where {T}
    !isempty(all) && !has_meta(all) && error("Profile data is missing required metadata")
    parent = root
    tops = Vector{StackFrameTree{T}}()
    build = Vector{StackFrameTree{T}}()
    startframe = length(all)
    skip = false
    nsleeping = 0
    is_task_profile = false
    is_sleeping = true
    for i in startframe:-1:1
        (startframe - 1) >= i >= (startframe - (nmeta + 1)) && continue # skip metadata (it's read ahead below) and extra block end NULL IP
        ip = all[i]
        if is_block_end(all, i)
            # read metadata
            thread_sleeping_state = all[i - META_OFFSET_SLEEPSTATE] - 1 # subtract 1 as state is incremented to avoid being equal to 0
            is_sleeping = thread_sleeping_state == 1
            is_task_profile = thread_sleeping_state == 2
            # cpu_cycle_clock = all[i - META_OFFSET_CPUCYCLECLOCK]
            taskid = all[i - META_OFFSET_TASKID]
            threadid = all[i - META_OFFSET_THREADID]
            if (threads !== nothing && !in(threadid, threads)) ||
               (tasks !== nothing && !in(taskid, tasks))
                skip = true
                continue
            end
            if thread_sleeping_state == 1
                nsleeping += 1
            end
            skip = false
            # sentinel value indicates the start of a new backtrace
            empty!(build)
            root.recur = 0
            if recur !== :off
                # We mark all visited nodes to so we'll only count those branches
                # once for each backtrace. Reset that now for the next backtrace.
                push!(tops, parent)
                for top in tops
                    while top.recur != 0
                        top.max_recur < top.recur && (top.max_recur = top.recur)
                        top.recur = 0
                        top = top.up
                    end
                end
                empty!(tops)
            end
            let this = parent
                while this !== root
                    this.flat_count += 1
                    this = this.up
                end
            end
            parent.overhead += 1
            parent = root
            root.count += 1
            startframe = i
        elseif !skip
            if recur === :flat || recur === :flatc
                pushfirst!(build, parent)
                # Rewind the `parent` tree back, if this exact ip was already present *higher* in the current tree
                found = false
                for j in 1:(startframe - i)
                    if ip == all[i + j]
                        if recur === :flat # if not flattening C frames, check that now
                            frames = lidict[ip]
                            frame = (frames isa Vector ? frames[1] : frames)
                            frame.from_c && break # not flattening this frame
                        end
                        push!(tops, parent)
                        parent = build[j]
                        parent.recur += 1
                        parent.count_recur += 1
                        parent.sleeping &= is_sleeping
                        found = true
                        break
                    end
                end
                found && continue
            end
            builder_key = parent.builder_key
            builder_value = parent.builder_value
            fastkey = searchsortedfirst(builder_key, ip)
            if fastkey < length(builder_key) && builder_key[fastkey] === ip
                # jump forward to the end of the inlining chain
                # avoiding an extra (slow) lookup of `ip` in `lidict`
                # and an extra chain of them in `down`
                # note that we may even have this === parent (if we're ignoring this frame ip)
                this = builder_value[fastkey]
                let this = this
                    while this !== parent && (recur === :off || this.recur == 0)
                        this.count += 1
                        this.recur = 1
                        this.sleeping &= is_sleeping
                        this = this.up
                    end
                end
                parent = this
                continue
            end

            frames = lidict[ip]
            nframes = (frames isa Vector ? length(frames) : 1)
            this = parent
            # add all the inlining frames
            for i = nframes:-1:1
                frame = (frames isa Vector ? frames[i] : frames)
                !C && frame.from_c && continue
                key = (T === UInt64 ? ip : frame)
                this = get!(StackFrameTree{T}, parent.down, key)
                if recur === :off || this.recur == 0
                    this.frame = frame
                    this.up = parent
                    this.count += 1
                    this.recur = 1
                    this.sleeping &= is_sleeping
                end
                parent = this
            end
            # record where the end of this chain is for this ip
            insert!(builder_key, fastkey, ip)
            insert!(builder_value, fastkey, this)
        end
    end
    function cleanup!(node::StackFrameTree)
        stack = [node]
        while !isempty(stack)
            node = pop!(stack)
            node.recur = 0
            empty!(node.builder_key)
            empty!(node.builder_value)
            append!(stack, values(node.down))
        end
        nothing
    end
    cleanup!(root)
    return root, nsleeping, is_task_profile
end

function maxstats(root::StackFrameTree)
    maxcount = Ref(0)
    maxflatcount = Ref(0)
    maxoverhead = Ref(0)
    maxmaxrecur = Ref(0)
    stack = [root]
    while !isempty(stack)
        node = pop!(stack)
        maxcount[] = max(maxcount[], node.count)
        maxoverhead[] = max(maxoverhead[], node.overhead)
        maxflatcount[] = max(maxflatcount[], node.flat_count)
        maxmaxrecur[] = max(maxmaxrecur[], node.max_recur)
        append!(stack, values(node.down))
    end
    return (count=maxcount[], count_flat=maxflatcount[], overhead=maxoverhead[], max_recur=maxmaxrecur[])
end

# Print the stack frame tree starting at a particular root. Uses a worklist to
# avoid stack overflows.
function print_tree(io::IO, bt::StackFrameTree{T}, cols::Int, fmt::ProfileFormat, is_subsection::Bool) where T
    maxes = maxstats(bt)
    filenamemap = FileNameMap()
    worklist = [(bt, 0, 0, AnnotatedString(""))]
    if !is_subsection
        Base.print(io, "Overhead ╎ [+additional indent] Count File:Line  Function\n")
        Base.print(io, "=========================================================\n")
    end
    while !isempty(worklist)
        (bt, level, noisefloor, str) = popfirst!(worklist)
        isempty(str) || println(io, str)
        level > fmt.maxdepth && continue
        isempty(bt.down) && continue
        # Order the line information
        nexts = collect(values(bt.down))
        # Generate the string for each line
        strs = tree_format(nexts, level, cols, maxes, filenamemap, T === UInt64)
        # Recurse to the next level
        if fmt.sortedby === :count
            counts = collect(frame.count for frame in nexts)
            p = sortperm(counts)
        elseif fmt.sortedby === :overhead
            m = collect(frame.overhead for frame in nexts)
            p = sortperm(m)
        elseif fmt.sortedby === :flat_count
            m = collect(frame.flat_count for frame in nexts)
            p = sortperm(m)
        else
            lilist = collect(frame.frame for frame in nexts)
            p = liperm(lilist)
        end
        for i in reverse(p)
            down = nexts[i]
            count = down.count
            count < fmt.mincount && continue
            count < noisefloor && continue
            str = strs[i]::AnnotatedString
            noisefloor_down = fmt.noisefloor > 0 ? floor(Int, fmt.noisefloor * sqrt(count)) : 0
            pushfirst!(worklist, (down, level + 1, noisefloor_down, str))
        end
    end
    return
end

function tree(io::IO, data::Vector{UInt64}, lidict::Union{LineInfoFlatDict, LineInfoDict}, cols::Int, fmt::ProfileFormat,
                threads::Union{Int,AbstractVector{Int}}, tasks::Union{UInt,AbstractVector{UInt}}, is_subsection::Bool)
    if fmt.combine
        root, nsleeping, is_task_profile = tree!(StackFrameTree{StackFrame}(), data, lidict, fmt.C, fmt.recur, threads, tasks)
    else
        root, nsleeping, is_task_profile = tree!(StackFrameTree{UInt64}(), data, lidict, fmt.C, fmt.recur, threads, tasks)
    end
    util_perc = (1 - (nsleeping / root.count)) * 100
    is_subsection || print_tree(io, root, cols, fmt, is_subsection)
    if isempty(root.down)
        if is_subsection
            Base.print(io, "Total snapshots: ")
            printstyled(io, "$(root.count)", color=Base.warn_color())
            Base.print(io, ". Utilization: ", round(Int, util_perc), "%\n")
        else
            warning_empty()
        end
        return true
    end
    if is_task_profile
        Base.print(io, "Total snapshots: ", root.count, "\n")
    else
        Base.print(io, "Total snapshots: ", root.count, ". Utilization: ", round(Int, util_perc), "%")
    end
    if is_subsection
        Base.println(io)
        print_tree(io, root, cols, fmt, is_subsection)
    elseif !is_task_profile
        Base.print(io, " across all threads and tasks. Use the `groupby` kwarg to break down by thread and/or task.\n")
    end
    return false
end

function callersf(matchfunc::Function, bt::Vector, lidict::LineInfoFlatDict)
    counts = Dict{StackFrame, Int}()
    lastmatched = false
    for id in bt
        if id == 0
            lastmatched = false
            continue
        end
        li = lidict[id]
        if lastmatched
            if haskey(counts, li)
                counts[li] += 1
            else
                counts[li] = 1
            end
        end
        lastmatched = matchfunc(li)
    end
    k = collect(keys(counts))
    v = collect(values(counts))
    p = sortperm(v, rev=true)
    return [(v[i], k[i]) for i in p]
end

## Utilities

# Order alphabetically (file, function) and then by line number
function liperm(lilist::Vector{StackFrame})
    function lt(a::StackFrame, b::StackFrame)
        a == UNKNOWN && return false
        b == UNKNOWN && return true
        fcmp = cmp(a.file, b.file)
        fcmp < 0 && return true
        fcmp > 0 && return false
        fcmp = cmp(a.func, b.func)
        fcmp < 0 && return true
        fcmp > 0 && return false
        fcmp = cmp(a.line, b.line)
        fcmp < 0 && return true
        return false
    end
    return sortperm(lilist, lt = lt)
end

function warning_empty(;summary = false)
    if summary
        @warn """
        There were no samples collected in one or more groups.
        This may be due to idle threads, or you may need to run your
        program longer (perhaps by running it multiple times),
        or adjust the delay between samples with `Profile.init()`."""
    else
        @warn """
        There were no samples collected.
        Run your program longer (perhaps by running it multiple times),
        or adjust the delay between samples with `Profile.init()`."""
    end
end


"""
    Profile.take_heap_snapshot(filepath::String, all_one::Bool=false;
                               redact_data::Bool=true, streaming::Bool=false)
    Profile.take_heap_snapshot(all_one::Bool=false; redact_data:Bool=true,
                               dir::String=nothing, streaming::Bool=false)

Write a snapshot of the heap, in the JSON format expected by the Chrome
Devtools Heap Snapshot viewer (.heapsnapshot extension) to a file
(`\$pid_\$timestamp.heapsnapshot`) in the current directory by default (or tempdir if
the current directory is unwritable), or in `dir` if given, or the given
full file path, or IO stream.

If `all_one` is true, then report the size of every object as one so they can be easily
counted. Otherwise, report the actual size.

If `redact_data` is true (default), then do not emit the contents of any object.

If `streaming` is true, we will stream the snapshot data out into four files, using filepath
as the prefix, to avoid having to hold the entire snapshot in memory. This option should be
used for any setting where your memory is constrained. These files can then be reassembled
by calling Profile.HeapSnapshot.assemble_snapshot(), which can
be done offline.

NOTE: We strongly recommend setting streaming=true for performance reasons. Reconstructing
the snapshot from the parts requires holding the entire snapshot in memory, so if the
snapshot is large, you can run out of memory while processing it. Streaming allows you to
reconstruct the snapshot offline, after your workload is done running.
If you do attempt to collect a snapshot with streaming=false (the default, for
backwards-compatibility) and your process is killed, note that this will always save the
parts in the same directory as your provided filepath, so you can still reconstruct the
snapshot after the fact, via `assemble_snapshot()`.
"""
function take_heap_snapshot(filepath::AbstractString, all_one::Bool=false; redact_data::Bool=true, streaming::Bool=false)
    if streaming
        _stream_heap_snapshot(filepath, all_one, redact_data)
    else
        # Support the legacy, non-streaming mode, by first streaming the parts, then
        # reassembling it after we're done.
        prefix = filepath
        _stream_heap_snapshot(prefix, all_one, redact_data)
        Profile.HeapSnapshot.assemble_snapshot(prefix, filepath)
        Profile.HeapSnapshot.cleanup_streamed_files(prefix)
    end
    return filepath
end
function take_heap_snapshot(io::IO, all_one::Bool=false; redact_data::Bool=true)
    # Support the legacy, non-streaming mode, by first streaming the parts to a tempdir,
    # then reassembling it after we're done.
    dir = tempdir()
    prefix = joinpath(dir, "snapshot")
    _stream_heap_snapshot(prefix, all_one, redact_data)
    Profile.HeapSnapshot.assemble_snapshot(prefix, io)
end
function _stream_heap_snapshot(prefix::AbstractString, all_one::Bool, redact_data::Bool)
    # Nodes and edges are binary files
    open("$prefix.nodes", "w") do nodes
        open("$prefix.edges", "w") do edges
            open("$prefix.strings", "w") do strings
                # The following file is json data
                open("$prefix.metadata.json", "w") do json
                    Base.@_lock_ios(nodes,
                    Base.@_lock_ios(edges,
                    Base.@_lock_ios(strings,
                    Base.@_lock_ios(json,
                        ccall(:jl_gc_take_heap_snapshot,
                            Cvoid,
                            (Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid}, Cchar, Cchar),
                            nodes.handle, edges.handle, strings.handle, json.handle,
                            Cchar(all_one), Cchar(redact_data))
                    )
                    )
                    )
                    )
                end
            end
        end
    end
end
function take_heap_snapshot(all_one::Bool=false; dir::Union{Nothing,S}=nothing, kwargs...) where {S <: AbstractString}
    fname = "$(getpid())_$(time_ns()).heapsnapshot"
    if isnothing(dir)
        wd = pwd()
        fpath = joinpath(wd, fname)
        try
            touch(fpath)
            rm(fpath; force=true)
        catch
            @warn "Cannot write to current directory `$(pwd())` so saving heap snapshot to `$(tempdir())`" maxlog=1 _id=Symbol(wd)
            fpath = joinpath(tempdir(), fname)
        end
    else
        fpath = joinpath(expanduser(dir), fname)
    end
    return take_heap_snapshot(fpath, all_one; kwargs...)
end

"""
    Profile.take_page_profile(io::IOStream)
    Profile.take_page_profile(filepath::String)

Write a JSON snapshot of the pages from Julia's pool allocator, printing for every pool allocated object, whether it's garbage, or its type.
"""
function take_page_profile(io::IOStream)
    Base.@_lock_ios(io, ccall(:jl_gc_take_page_profile, Cvoid, (Ptr{Cvoid},), io.handle))
end
function take_page_profile(filepath::String)
    open(filepath, "w") do io
        take_page_profile(io)
    end
    return filepath
end

include("Allocs.jl")
include("heapsnapshot_reassemble.jl")
include("precompile.jl")

end # module
