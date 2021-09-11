# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Profiling support, main entry point is the [`@profile`](@ref) macro.
"""
module Profile

import Base.StackTraces: lookup, UNKNOWN, show_spec_linfo, StackFrame

const nmeta = 4 # number of metadata fields per block (threadid, taskid, cpu_cycle_clock, thread_sleeping)

# deprecated functions: use `getdict` instead
lookup(ip::UInt) = lookup(convert(Ptr{Cvoid}, ip))

export @profile

"""
    @profile

`@profile <expression>` runs your expression while taking periodic backtraces. These are
appended to an internal buffer of backtraces.
"""
macro profile(ex)
    return quote
        try
            status = start_timer()
            if status < 0
                error(error_codes[status])
            end
            $(esc(ex))
        finally
            stop_timer()
        end
    end
end

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

!!! compat "Julia 1.8"
    As of Julia 1.8, this function allocates space for `n` instruction pointers per thread being profiled.
    Previously this was `n` total.
"""
function init(; n::Union{Nothing,Integer} = nothing, delay::Union{Nothing,Real} = nothing, limitwarn::Bool = true)
    n_cur = ccall(:jl_profile_maxlen_data, Csize_t, ())
    delay_cur = ccall(:jl_profile_delay_nsec, UInt64, ())/10^9
    if n === nothing && delay === nothing
        return Int(n_cur), delay_cur
    end
    nnew = (n === nothing) ? n_cur : n
    delaynew = (delay === nothing) ? delay_cur : delay
    init(nnew, delaynew; limitwarn)
end

function init(n::Integer, delay::Real; limitwarn::Bool = true)
    nthreads = Sys.iswindows() ? 1 : Threads.nthreads() # windows only profiles the main thread
    sample_size_bytes = sizeof(Ptr) # == Sys.WORD_SIZE / 8
    buffer_samples = n * nthreads
    buffer_size_bytes = buffer_samples * sample_size_bytes
    if buffer_size_bytes > 2^29 && Sys.WORD_SIZE == 32
        buffer_size_bytes_per_thread = floor(Int, 2^29 / nthreads)
        buffer_samples_per_thread = floor(Int, buffer_size_bytes_per_thread / sample_size_bytes)
        buffer_samples = buffer_samples_per_thread * nthreads
        buffer_size_bytes = buffer_samples * sample_size_bytes
        limitwarn && @warn "Requested profile buffer limited to 512MB (n = $buffer_samples_per_thread per thread) given that this system is 32-bit"
    end
    status = ccall(:jl_profile_init, Cint, (Csize_t, UInt64), buffer_samples, round(UInt64,10^9*delay))
    if status == -1
        error("could not allocate space for ", n, " instruction pointers per thread being profiled ($nthreads threads, $(Base.format_bytes(buffer_size_bytes)) total)")
    end
end

# init with default values
# Use a max size of 10M profile samples, and fire timer every 1ms
# (that should typically give around 100 seconds of record)
if Sys.iswindows() && Sys.WORD_SIZE == 32
    # The Win32 unwinder is 1000x slower than elsewhere (around 1ms/frame),
    # so we don't want to slow the program down by quite that much
    __init__() = init(1_000_000, 0.01, limitwarn = false)
else
    __init__() = init(10_000_000, 0.001, limitwarn = false)
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

"""
    print([io::IO = stdout,] [data::Vector]; kwargs...)

Prints profiling results to `io` (by default, `stdout`). If you do not
supply a `data` vector, the internal buffer of accumulated backtraces
will be used.

The keyword arguments can be any combination of:

 - `format` -- Determines whether backtraces are printed with (default, `:tree`) or without (`:flat`)
   indentation indicating tree structure.

 - `C` -- If `true`, backtraces from C and Fortran code are shown (normally they are excluded).

 - `combine` -- If `true` (default), instruction pointers are merged that correspond to the same line of code.

 - `maxdepth` -- Limits the depth higher than `maxdepth` in the `:tree` format.

 - `sortedby` -- Controls the order in `:flat` format. `:filefuncline` (default) sorts by the source
    line, `:count` sorts in order of number of collected samples, and `:overhead` sorts by the number of samples
    incurred by each function by itself.

 - `groupby` -- Controls grouping over tasks and threads, or no grouping. Options are `:none` (default), `:threads`, `:tasks`,
    `[:threads, :tasks]`, or `[:tasks, :threads]` where the last two provide nested grouping.

 - `noisefloor` -- Limits frames that exceed the heuristic noise floor of the sample (only applies to format `:tree`).
    A suggested value to try for this is 2.0 (the default is 0). This parameter hides samples for which `n <= noisefloor * √N`,
    where `n` is the number of samples on this line, and `N` is the number of samples for the callee.

 - `mincount` -- Limits the printout to only those lines with at least `mincount` occurrences.

 - `recur` -- Controls the recursion handling in `:tree` format. `:off` (default) prints the tree as normal. `:flat` instead
    compresses any recursion (by ip), showing the approximate effect of converting any self-recursion into an iterator.
    `:flatc` does the same but also includes collapsing of C frames (may do odd things around `jl_apply`).

 - `threads::Union{Int,AbstractVector{Int}}` -- Specify which threads to include snapshots from in the report. Note that
    this does not control which threads samples are collected on.

 - `tasks::Union{Int,AbstractVector{Int}}` -- Specify which tasks to include snapshots from in the report. Note that this
    does not control which tasks samples are collected within.
"""
function print(io::IO,
        data::Vector{<:Unsigned} = fetch(include_meta = true),
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
        threads::Union{Int,AbstractVector{Int}} = 1:Threads.nthreads(),
        tasks::Union{UInt,AbstractVector{UInt}} = typemin(UInt):typemax(UInt))

    pf = ProfileFormat(;C, combine, maxdepth, mincount, noisefloor, sortedby, recur)
    if groupby == :none
        print(io, data, lidict, pf, format, threads, tasks, false)
    else
        if !in(groupby, [:thread, :task, [:task, :thread], [:thread, :task]])
            error(ArgumentError("Unrecognized groupby option: $groupby. Options are :none (default), :task, :thread, [:task, :thread], or [:thread, :task]"))
        elseif Sys.iswindows() && in(groupby, [:thread, [:task, :thread], [:thread, :task]])
            @warn "Profiling on windows is limited to the main thread. Other threads have not been sampled and will not show in the report"
        end
        any_nosamples = false
        println(io, "Overhead ╎ [+additional indent] Count File:Line; Function")
        println(io, "=========================================================")
        if groupby == [:task, :thread]
            for taskid in intersect(get_task_ids(data), tasks)
                threadids = intersect(get_thread_ids(data, taskid), threads)
                if length(threadids) == 0
                    any_nosamples = true
                else
                    nl = length(threadids) > 1 ? "\n" : ""
                    printstyled(io, "Task $(Base.repr(taskid))$nl"; bold=true, color=Base.debug_color())
                    for threadid in threadids
                        printstyled(io, " Thread $threadid\n"; bold=true, color=Base.info_color())
                        nosamples = print(io, data, lidict, pf, format, threadid, taskid, true)
                        nosamples && (any_nosamples = true)
                        println(io)
                    end
                end
            end
        elseif groupby == [:thread, :task]
            for threadid in intersect(get_thread_ids(data), threads)
                taskids = intersect(get_task_ids(data, threadid), tasks)
                if length(taskids) == 0
                    any_nosamples = true
                else
                    nl = length(taskids) > 1 ? "\n" : ""
                    printstyled(io, "Thread $threadid$nl"; bold=true, color=Base.info_color())
                    for taskid in taskids
                        printstyled(io, " Task $(Base.repr(taskid))\n"; bold=true, color=Base.debug_color())
                        nosamples = print(io, data, lidict, pf, format, threadid, taskid, true)
                        nosamples && (any_nosamples = true)
                        println(io)
                    end
                end
            end
        elseif groupby == :task
            threads = 1:typemax(Int)
            for taskid in intersect(get_task_ids(data), tasks)
                printstyled(io, "Task $(Base.repr(taskid))\n"; bold=true, color=Base.debug_color())
                nosamples = print(io, data, lidict, pf, format, threads, taskid, true)
                nosamples && (any_nosamples = true)
                println(io)
            end
        elseif groupby == :thread
            tasks = 1:typemax(UInt)
            for threadid in intersect(get_thread_ids(data), threads)
                printstyled(io, "Thread $threadid\n"; bold=true, color=Base.info_color())
                nosamples = print(io, data, lidict, pf, format, threadid, tasks, true)
                nosamples && (any_nosamples = true)
                println(io)
            end
        end
        any_nosamples && warning_empty(summary = true)
    end
    return
end

function print(io::IO, data::Vector{<:Unsigned}, lidict::Union{LineInfoDict, LineInfoFlatDict}, fmt::ProfileFormat,
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
            if isnothing(threadid) || data[i - 5] == threadid
                taskid = data[i - 4]
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
            if isnothing(taskid) || data[i - 4] == taskid
                threadid = data[i - 5]
                !in(threadid, threadids) && push!(threadids, threadid)
            end
        end
    end
    return sort(threadids)
end

function is_block_end(data, i)
    i < nmeta + 1 && return false
    # 32-bit linux has been seen to have rogue NULL ips, so we use two to indicate block end, where the 2nd is the
    # actual end index
    return data[i] == 0 && data[i - 1] == 0
end

"""
    print([io::IO = stdout,] data::Vector, lidict::LineInfoDict; kwargs...)

Prints profiling results to `io`. This variant is used to examine results exported by a
previous call to [`retrieve`](@ref). Supply the vector `data` of backtraces and
a dictionary `lidict` of line information.

See `Profile.print([io], data)` for an explanation of the valid keyword arguments.
"""
print(data::Vector{<:Unsigned} = fetch(include_meta = true), lidict::Union{LineInfoDict, LineInfoFlatDict} = getdict(data); kwargs...) =
    print(stdout, data, lidict; kwargs...)

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
    for ip in data
        # Lookup is expensive, so do it only once per ip.
        haskey(dict, UInt64(ip)) && continue
        st = lookup(convert(Ptr{Cvoid}, ip))
        # To correct line numbers for moving code, put it in the form expected by
        # Base.update_stackframes_callback[]
        stn = map(x->(x, 1), st)
        try Base.invokelatest(Base.update_stackframes_callback[], stn) catch end
        dict[UInt64(ip)] = map(first, stn)
    end
    return dict
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

# Take a file-system path and try to form a concise representation of it
# based on the package ecosystem
function short_path(spath::Symbol, filenamecache::Dict{Symbol, String})
    return get!(filenamecache, spath) do
        path = string(spath)
        if isabspath(path)
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
                            isempty(pkgid.name) && return path # bad Project file
                            # return the joined the module name prefix and path suffix
                            path = path[nextind(path, sizeof(root)):end]
                            return string("@", pkgid.name, path)
                        end
                    end
                end
            end
            return path
        elseif isfile(joinpath(Sys.BINDIR::String, Base.DATAROOTDIR, "julia", "base", path))
            # do the same mechanic for Base (or Core/Compiler) files as above,
            # but they start from a relative path
            return joinpath("@Base", normpath(path))
        else
            # for non-existent relative paths (such as "REPL[1]"), just consider simplifying them
            return normpath(path) # drop leading "./"
        end
    end
end

"""
    callers(funcname, [data, lidict], [filename=<filename>], [linerange=<start:stop>]) -> Vector{Tuple{count, lineinfo}}

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
start_timer() = ccall(:jl_profile_start_timer, Cint, ())

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
    fetch(;include_meta = false) -> data

Returns a copy of the buffer of profile backtraces. Note that the
values in `data` have meaning only on this machine in the current session, because it
depends on the exact memory addresses used in JIT-compiling. This function is primarily for
internal use; [`retrieve`](@ref) may be a better choice for most users.
By default metadata such as threadid and taskid will be stripped. Set `include_meta` to `true` to include metadata.
"""
function fetch(;include_meta = false)
    maxlen = maxlen_data()
    len = len_data()
    if is_buffer_full()
        @warn """The profile data buffer is full; profiling probably terminated
                 before your program finished. To profile for longer runs, call
                 `Profile.init()` with a larger buffer and/or larger delay."""
    end
    data = Vector{UInt}(undef, len)
    GC.@preserve data unsafe_copyto!(pointer(data), get_data_pointer(), len)
    if include_meta || isempty(data)
        return data
    else
        nblocks = 0
        for i = 2:length(data)
            if is_block_end(data, i) # detect block ends and count them
                nblocks += 1
            end
        end
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
        @assert i == j == 0 "metadata stripping failed i=$i j=$j data[1:i]=$(data[1:i])"
        return data_stripped
    end
end


## Print as a flat list
# Counts the number of times each line appears, at any nesting level and at the topmost level
# Merging multiple equivalent entries and recursive calls
function parse_flat(::Type{T}, data::Vector{UInt64}, lidict::Union{LineInfoDict, LineInfoFlatDict}, C::Bool,
                    threads::Union{Int,AbstractVector{Int}}, tasks::Union{UInt,AbstractVector{UInt}}) where {T}
    lilist = StackFrame[]
    n = Int[]
    m = Int[]
    lilist_idx = Dict{T, Int}()
    recursive = Set{T}()
    first = true
    totalshots = 0
    startframe = length(data)
    skip = false
    nsleeping = 0
    for i in startframe:-1:1
        startframe - 1 <= i <= startframe - (nmeta + 1) && continue # skip metadata (it's read ahead below) and extra block-end NULL IP
        ip = data[i]
        if is_block_end(data, i)
            # read metadata
            thread_sleeping = data[i - 2] - 1 # subtract 1 as state is incremented to avoid being equal to 0
            # cpu_cycle_clock = data[i - 3]
            taskid = data[i - 4]
            threadid = data[i - 5]
            if !in(threadid, threads) || !in(taskid, tasks)
                skip = true
                continue
            end
            if thread_sleeping == 1
                nsleeping += 1
            end
            skip = false
            totalshots += 1
            empty!(recursive)
            first = true
            startframe = i
        elseif !skip
            frames = lidict[ip]
            nframes = (frames isa Vector ? length(frames) : 1)
            for j = 1:nframes
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
                if first
                    m[idx] += 1
                    first = false
                end
            end
        end
    end
    @assert length(lilist) == length(n) == length(m) == length(lilist_idx)
    return (lilist, n, m, totalshots, nsleeping)
end

function flat(io::IO, data::Vector{UInt64}, lidict::Union{LineInfoDict, LineInfoFlatDict}, cols::Int, fmt::ProfileFormat,
                threads::Union{Int,AbstractVector{Int}}, tasks::Union{UInt,AbstractVector{UInt}}, is_subsection::Bool)
    lilist, n, m, totalshots, nsleeping = parse_flat(fmt.combine ? StackFrame : UInt64, data, lidict, fmt.C, threads, tasks)
    util_perc = (1 - (nsleeping / totalshots)) * 100
    if isempty(lilist)
        if is_subsection
            Base.print(io, "Total snapshots: ")
            printstyled(io, "$(totalshots)", color=Base.warn_color())
            Base.println(io, " (", round(Int, util_perc), "% utilization)")
        else
            warning_empty()
        end
        return true
    end
    if false # optional: drop the "non-interpretable" ones
        keep = map(frame -> frame != UNKNOWN && frame.line != 0, lilist)
        lilist = lilist[keep]
        n = n[keep]
        m = m[keep]
    end
    filenamemap = Dict{Symbol,String}()
    print_flat(io, lilist, n, m, cols, filenamemap, fmt)
    Base.print(io, "Total snapshots: ", totalshots, " (", round(Int, util_perc), "% utilization")
    if is_subsection
        println(io, ")")
    else
        println(io, " across all threads and tasks. Use the `groupby` kwarg to break down by thread and/or task)")
    end
    return false
end

function print_flat(io::IO, lilist::Vector{StackFrame},
        n::Vector{Int}, m::Vector{Int},
        cols::Int, filenamemap::Dict{Symbol,String},
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
    filenames = String[short_path(li.file, filenamemap) for li in lilist]
    funcnames = String[string(li.func) for li in lilist]
    wcounts = max(6, ndigits(maximum(n)))
    wself = max(9, ndigits(maximum(m)))
    maxline = 1
    maxfile = 6
    maxfunc = 10
    for i in 1:length(lilist)
        li = lilist[i]
        maxline = max(maxline, li.line)
        maxfunc = max(maxfunc, length(funcnames[i]))
        maxfile = max(maxfile, length(filenames[i]))
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
    for i = 1:length(n)
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
            file = filenames[i]
            isempty(file) && (file = "[unknown file]")
            Base.print(io, rpad(rtruncto(file, wfile), wfile, " "), " ")
            Base.print(io, lpad(li.line > 0 ? string(li.line) : "?", wline, " "), " ")
            fname = funcnames[i]
            if !li.from_c && li.linfo !== nothing
                fname = sprint(show_spec_linfo, li)
            end
            isempty(fname) && (fname = "[unknown function]")
            Base.print(io, ltruncto(fname, wfunc))
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
    down::Dict{T, StackFrameTree{T}}
    # construction workers:
    recur::Int
    builder_key::Vector{UInt64}
    builder_value::Vector{StackFrameTree{T}}
    up::StackFrameTree{T}
    StackFrameTree{T}() where {T} = new(UNKNOWN, 0, 0, 0, 0, 0, Dict{T, StackFrameTree{T}}(), 0, UInt64[], StackFrameTree{T}[])
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

function tree_format(frames::Vector{<:StackFrameTree}, level::Int, cols::Int, maxes, filenamemap::Dict{Symbol,String}, showpointer::Bool)
    nindent = min(cols>>1, level)
    ndigoverhead = ndigits(maxes.overhead)
    ndigcounts = ndigits(maxes.count)
    ndigline = ndigits(maximum(frame.frame.line for frame in frames)) + 6
    ntext = max(30, cols - ndigoverhead - nindent - ndigcounts - ndigline - 6)
    widthfile = 2*ntext÷5 # min 12
    widthfunc = 3*ntext÷5 # min 18
    strs = Vector{String}(undef, length(frames))
    showextra = false
    if level > nindent
        nextra = level - nindent
        nindent -= ndigits(nextra) + 2
        showextra = true
    end
    for i = 1:length(frames)
        frame = frames[i]
        li = frame.frame
        stroverhead = lpad(frame.overhead > 0 ? string(frame.overhead) : "", ndigoverhead, " ")
        base = nindent == 0 ? "" : indent(nindent - 1) * " "
        if showextra
            base = string(base, "+", nextra, " ")
        end
        strcount = rpad(string(frame.count), ndigcounts, " ")
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
                filename = short_path(li.file, filenamemap)
                if showpointer
                    fname = string(
                        "0x",
                        string(li.pointer, base = 16, pad = 2*sizeof(Ptr{Cvoid})),
                        " ",
                        fname)
                end
                strs[i] = string(stroverhead, "╎", base, strcount, " ",
                    rtruncto(filename, widthfile),
                    ":",
                    li.line == -1 ? "?" : string(li.line),
                    "; ",
                    ltruncto(fname, widthfunc))
            end
        else
            strs[i] = string(stroverhead, "╎", base, strcount, " [unknown stackframe]")
        end
    end
    return strs
end

# turn a list of backtraces into a tree (implicitly separated by NULL markers)
function tree!(root::StackFrameTree{T}, all::Vector{UInt64}, lidict::Union{LineInfoFlatDict, LineInfoDict}, C::Bool, recur::Symbol,
                threads::Union{Int,AbstractVector{Int}}, tasks::Union{UInt,AbstractVector{UInt}}) where {T}
    parent = root
    tops = Vector{StackFrameTree{T}}()
    build = Vector{StackFrameTree{T}}()
    startframe = length(all)
    skip = false
    nsleeping = 0
    for i in startframe:-1:1
        startframe - 1 <= i <= startframe - (nmeta + 1) && continue # skip metadata (its read ahead below) and extra block end NULL IP
        ip = all[i]
        if is_block_end(all, i)
            # read metadata
            thread_sleeping = all[i - 2] - 1 # subtract 1 as state is incremented to avoid being equal to 0
            # cpu_cycle_clock = all[i - 3]
            taskid = all[i - 4]
            threadid = all[i - 5]
            if !in(threadid, threads) || !in(taskid, tasks)
                skip = true
                continue
            end
            if thread_sleeping == 1
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
            pushfirst!(build, parent)
            if recur === :flat || recur === :flatc
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
    return root, nsleeping
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
    filenamemap = Dict{Symbol,String}()
    worklist = [(bt, 0, 0, "")]
    if !is_subsection
        println(io, "Overhead ╎ [+additional indent] Count File:Line; Function")
        println(io, "=========================================================")
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
            str = strs[i]
            noisefloor_down = fmt.noisefloor > 0 ? floor(Int, fmt.noisefloor * sqrt(count)) : 0
            pushfirst!(worklist, (down, level + 1, noisefloor_down, str))
        end
    end
    return
end

function tree(io::IO, data::Vector{UInt64}, lidict::Union{LineInfoFlatDict, LineInfoDict}, cols::Int, fmt::ProfileFormat,
                threads::Union{Int,AbstractVector{Int}}, tasks::Union{UInt,AbstractVector{UInt}}, is_subsection::Bool)
    if fmt.combine
        root, nsleeping = tree!(StackFrameTree{StackFrame}(), data, lidict, fmt.C, fmt.recur, threads, tasks)
    else
        root, nsleeping = tree!(StackFrameTree{UInt64}(), data, lidict, fmt.C, fmt.recur, threads, tasks)
    end
    util_perc = (1 - (nsleeping / root.count)) * 100
    if isempty(root.down)
        if is_subsection
            Base.print(io, "Total snapshots: ")
            printstyled(io, "$(root.count)", color=Base.warn_color())
            Base.println(io, " (", round(Int, util_perc), "% utilization)")
        else
            warning_empty()
        end
        return true
    end
    print_tree(io, root, cols, fmt, is_subsection)
    Base.print(io, "Total snapshots: ", root.count, " (", round(Int, util_perc), "% utilization")
    if is_subsection
        println(io, ")")
    else
        println(io, " across all threads and tasks. Use the `groupby` kwarg to break down by thread and/or task)")
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

# Utilities
function rtruncto(str::String, w::Int)
    if length(str) <= w
        return str
    else
        return string("...", str[prevind(str, end, w-4):end])
    end
end
function ltruncto(str::String, w::Int)
    if length(str) <= w
        return str
    else
        return string(str[1:nextind(str, 1, w-4)], "...")
    end
end


truncto(str::Symbol, w::Int) = truncto(string(str), w)

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

end # module
