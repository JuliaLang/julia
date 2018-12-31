# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Profiling support, main entry point is the [`@profile`](@ref) macro.
"""
module Profile

import Base.StackTraces: lookup, UNKNOWN, show_spec_linfo, StackFrame

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
    init(; n::Integer, delay::Real))

Configure the `delay` between backtraces (measured in seconds), and the number `n` of
instruction pointers that may be stored. Each instruction pointer corresponds to a single
line of code; backtraces generally consist of a long list of instruction pointers. Default
settings can be obtained by calling this function with no arguments, and each can be set
independently using keywords or in the order `(n, delay)`.
"""
function init(; n::Union{Nothing,Integer} = nothing, delay::Union{Nothing,Real} = nothing)
    n_cur = ccall(:jl_profile_maxlen_data, Csize_t, ())
    delay_cur = ccall(:jl_profile_delay_nsec, UInt64, ())/10^9
    if n === nothing && delay === nothing
        return Int(n_cur), delay_cur
    end
    nnew = (n === nothing) ? n_cur : n
    delaynew = (delay === nothing) ? delay_cur : delay
    init(nnew, delaynew)
end

function init(n::Integer, delay::Real)
    status = ccall(:jl_profile_init, Cint, (Csize_t, UInt64), n, round(UInt64,10^9*delay))
    if status == -1
        error("could not allocate space for ", n, " instruction pointers")
    end
end

# init with default values
# Use a max size of 1M profile samples, and fire timer every 1ms
if Sys.iswindows()
    __init__() = init(1_000_000, 0.01)
elseif !Sys.isjsvm()
    __init__() = init(1_000_000, 0.001)
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
    function ProfileFormat(;
        C = false,
        combine = true,
        maxdepth::Int = typemax(Int),
        mincount::Int = 0,
        noisefloor = 0,
        sortedby::Symbol = :filefuncline)
        return new(maxdepth, mincount, noisefloor, sortedby, combine, C)
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
    line, whereas `:count` sorts in order of number of collected samples.

 - `noisefloor` -- Limits frames that exceed the heuristic noise floor of the sample (only applies to format `:tree`).
    A suggested value to try for this is 2.0 (the default is 0). This parameter hides samples for which `n <= noisefloor * √N`,
    where `n` is the number of samples on this line, and `N` is the number of samples for the callee.

 - `mincount` -- Limits the printout to only those lines with at least `mincount` occurrences.
"""
function print(io::IO, data::Vector{<:Unsigned} = fetch(), lidict::Union{LineInfoDict, LineInfoFlatDict} = getdict(data);
        format = :tree,
        C = false,
        combine = true,
        maxdepth::Int = typemax(Int),
        mincount::Int = 0,
        noisefloor = 0,
        sortedby::Symbol = :filefuncline)
    print(io, data, lidict, ProfileFormat(C = C,
            combine = combine,
            maxdepth = maxdepth,
            mincount = mincount,
            noisefloor = noisefloor,
            sortedby = sortedby),
        format)
end

function print(io::IO, data::Vector{<:Unsigned}, lidict::Union{LineInfoDict, LineInfoFlatDict}, fmt::ProfileFormat, format::Symbol)
    cols::Int = Base.displaysize(io)[2]
    data = convert(Vector{UInt64}, data)
    if format == :tree
        tree(io, data, lidict, cols, fmt)
    elseif format == :flat
        flat(io, data, lidict, cols, fmt)
    else
        throw(ArgumentError("output format $(repr(format)) not recognized"))
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

"""
    retrieve() -> data, lidict

"Exports" profiling results in a portable format, returning the set of all backtraces
(`data`) and a dictionary that maps the (session-specific) instruction pointers in `data` to
`LineInfo` values that store the file name, function name, and line number. This function
allows you to save profiling results for future analysis.
"""
function retrieve()
    data = fetch()
    return (copy(data), getdict(data))
end

function getdict(data::Vector{UInt})
    uip = unique(data)
    return LineInfoDict(UInt64(ip)=>lookup(ip) for ip in uip)
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

get_data_pointer() = convert(Ptr{UInt}, ccall(:jl_profile_get_data, Ptr{UInt8}, ()))

len_data() = convert(Int, ccall(:jl_profile_len_data, Csize_t, ()))

maxlen_data() = convert(Int, ccall(:jl_profile_maxlen_data, Csize_t, ()))

error_codes = Dict(
    -1=>"cannot specify signal action for profiling",
    -2=>"cannot create the timer for profiling",
    -3=>"cannot start the timer for profiling",
    -4=>"cannot unblock SIGUSR1")


"""
    fetch() -> data

Returns a reference to the internal buffer of backtraces. Note that subsequent operations,
like [`clear`](@ref), can affect `data` unless you first make a copy. Note that the
values in `data` have meaning only on this machine in the current session, because it
depends on the exact memory addresses used in JIT-compiling. This function is primarily for
internal use; [`retrieve`](@ref) may be a better choice for most users.
"""
function fetch()
    len = len_data()
    maxlen = maxlen_data()
    if (len == maxlen)
        @warn """The profile data buffer is full; profiling probably terminated
                 before your program finished. To profile for longer runs, call
                 `Profile.init()` with a larger buffer and/or larger delay."""
    end
    return unsafe_wrap(Array, get_data_pointer(), (len,))
end


## Print as a flat list
# Counts the number of times each line appears, at any nesting level
function count_flat(data::Vector{UInt64})
    linecount = Dict{UInt64, Int}()
    for ip in data
        if ip != 0
            linecount[ip] = get(linecount, ip, 0) + 1
        end
    end
    iplist = Vector{UInt64}()
    n = Vector{Int}()
    for (k, v) in linecount
        push!(iplist, k)
        push!(n, v)
    end
    return (iplist, n)
end

function parse_flat(iplist::Vector{UInt64}, n::Vector{Int}, lidict::Union{LineInfoDict, LineInfoFlatDict}, C::Bool)
    # Convert instruction pointers to names & line numbers
    lilist = StackFrame[]
    nlist = Int[]
    for (ip, count) in zip(iplist, n)
        frames = lidict[ip]
        nframes = (frames isa Vector ? length(frames) : 1)
        # add all the inlining frames
        for i = nframes:-1:1
            frame = (frames isa Vector ? frames[i] : frames)
            # Keep only the interpretable ones
            # The ones with no line number might appear multiple times in a single
            # backtrace, giving the wrong impression about the total number of backtraces.
            # Delete them too.
            if frame != UNKNOWN && frame.line != 0 && (C || !frame.from_c)
                push!(lilist, frame)
                push!(nlist, count)
            end
        end
    end
    return (lilist, nlist)
end

function flat(io::IO, data::Vector{UInt64}, lidict::Union{LineInfoDict, LineInfoFlatDict}, cols::Int, fmt::ProfileFormat)
    iplist, n = count_flat(data)
    if isempty(n)
        warning_empty()
        return
    end
    lilist, n = parse_flat(iplist, n, lidict, fmt.C)
    print_flat(io, lilist, n, cols, fmt)
    nothing
end

function print_flat(io::IO, lilist::Vector{StackFrame}, n::Vector{Int},
        cols::Int, fmt::ProfileFormat)
    p = liperm(lilist)
    lilist = lilist[p]
    n = n[p]
    if fmt.combine
        # now that lilist is sorted by li,
        # combine adjacent entries that are equivlent
        j = 1
        for i = 2:length(lilist)
            if lilist[i] == lilist[j]
                n[j] += n[i]
                n[i] = 0
            else
                j = i
            end
        end
        keep = n .> 0
        n = n[keep]
        lilist = lilist[keep]
    end
    if fmt.sortedby == :count
        p = sortperm(n)
        n = n[p]
        lilist = lilist[p]
    end
    wcounts = max(6, ndigits(maximum(n)))
    maxline = 0
    maxfile = 6
    maxfunc = 10
    for li in lilist
        maxline = max(maxline, li.line)
        maxfile = max(maxfile, length(string(li.file)))
        maxfunc = max(maxfunc, length(string(li.func)))
    end
    wline = max(5, ndigits(maxline))
    ntext = cols - wcounts - wline - 3
    maxfunc += 25
    if maxfile + maxfunc <= ntext
        wfile = maxfile
        wfunc = maxfunc
    else
        wfile = floor(Integer, 2*ntext/5)
        wfunc = floor(Integer, 3*ntext/5)
    end
    println(io, lpad("Count", wcounts, " "), " ", rpad("File", wfile, " "), " ",
        lpad("Line", wline, " "), " ", rpad("Function", wfunc, " "))
    for i = 1:length(n)
        n[i] < fmt.mincount && continue
        li = lilist[i]
        Base.print(io, lpad(string(n[i]), wcounts, " "), " ")
        Base.print(io, rpad(rtruncto(string(li.file), wfile), wfile, " "), " ")
        Base.print(io, lpad(string(li.line), wline, " "), " ")
        fname = string(li.func)
        if !li.from_c && li.linfo !== nothing
            fname = sprint(show_spec_linfo, li)
        end
        Base.print(io, rpad(ltruncto(fname, wfunc), wfunc, " "))
        println(io)
    end
    nothing
end

## A tree representation
tree_format_linewidth(x::StackFrame) = ndigits(x.line) + 6

function tree_format(lilist::Vector{StackFrame}, counts::Vector{Int}, level::Int, cols::Int)
    nindent = min(cols>>1, level)
    ndigcounts = ndigits(maximum(counts))
    ndigline = maximum([tree_format_linewidth(x) for x in lilist])
    ntext = cols - nindent - ndigcounts - ndigline - 5
    widthfile = floor(Integer, 0.4ntext)
    widthfunc = floor(Integer, 0.6ntext)
    strs = Vector{String}(undef, length(lilist))
    showextra = false
    if level > nindent
        nextra = level - nindent
        nindent -= ndigits(nextra) + 2
        showextra = true
    end
    for i = 1:length(lilist)
        li = lilist[i]
        if li != UNKNOWN
            base = " "^nindent
            if showextra
                base = string(base, "+", nextra, " ")
            end
            if li.line == li.pointer
                strs[i] = string(base,
                    rpad(string(counts[i]), ndigcounts, " "),
                    " ",
                    "unknown function (pointer: 0x",
                    string(li.pointer, base = 16, pad = 2*sizeof(Ptr{Cvoid})),
                    ")")
            else
                fname = string(li.func)
                if !li.from_c && li.linfo !== nothing
                    fname = sprint(show_spec_linfo, li)
                end
                strs[i] = string(base,
                    rpad(string(counts[i]), ndigcounts, " "),
                    " ",
                    rtruncto(string(li.file), widthfile),
                    ":",
                    li.line == -1 ? "?" : string(li.line),
                    "; ",
                    ltruncto(fname, widthfunc))
            end
        else
            strs[i] = ""
        end
    end
    return strs
end

# Construct a prefix trie of backtrace counts
mutable struct StackFrameTree{T} # where T <: Union{UInt64, StackFrame}
    # content fields:
    frame::StackFrame
    count::Int
    down::Dict{T, StackFrameTree{T}}
    # construction helpers:
    builder_key::Vector{UInt64}
    builder_value::Vector{StackFrameTree{T}}
    up::StackFrameTree{T}
    StackFrameTree{T}() where {T} = new(UNKNOWN, 0, Dict{T, StackFrameTree{T}}(), UInt64[], StackFrameTree{T}[])
end

# turn a list of backtraces into a tree (implicitly separated by NULL markers)
function tree!(root::StackFrameTree{T}, all::Vector{UInt64}, lidict::Union{LineInfoFlatDict, LineInfoDict}, C::Bool) where {T}
    parent = root
    for i in length(all):-1:1
        ip = all[i]
        if ip == 0
            # sentinel value indicates the start of a new backtrace
            parent = root
            parent.count += 1
        else
            builder_key = parent.builder_key
            builder_value = parent.builder_value
            fastkey = searchsortedfirst(parent.builder_key, ip)
            if fastkey < length(builder_key) && builder_key[fastkey] === ip
                # jump forward to the end of the inlining chain
                # avoiding an extra (slow) lookup of `ip` in `lidict`
                # and an extra chain of them in `down`
                # note that we may even have this === parent (if we're ignoring this frame ip)
                this = builder_value[fastkey]
                let this = this
                    while this !== parent
                        this.count += 1
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
                this = get!(parent.down, key) do
                    return StackFrameTree{T}()
                end
                this.frame = frame
                this.up = parent
                this.count += 1
                parent = this
            end
            # record where the end of this chain is for this ip
            insert!(builder_key, fastkey, ip)
            insert!(builder_value, fastkey, this)
        end
    end
    function cleanup!(node::StackFrameTree)
        stack = StackFrameTree[node]
        while !isempty(stack)
            node = pop!(stack)
            empty!(node.builder_key)
            empty!(node.builder_value)
            append!(stack, values(node.down))
        end
        nothing
    end
    cleanup!(root)
    return root
end

# Print the stack frame tree starting at a particular root. Uses a worklist to
# avoid stack overflows.
function tree(io::IO, bt::StackFrameTree, cols::Int, fmt::ProfileFormat)
    worklist = [(bt, 0, 0, "")]
    while !isempty(worklist)
        (bt, level, noisefloor, str) = popfirst!(worklist)
        isempty(str) || println(io, str)
        level > fmt.maxdepth && continue
        isempty(bt.down) && continue
        # Order the line information
        nexts = collect(values(bt.down))
        lilist = collect(frame.frame for frame in nexts)
        counts = collect(frame.count for frame in nexts)
        # Generate the string for each line
        strs = tree_format(lilist, counts, level, cols)
        # Recurse to the next level
        for i in reverse(liperm(lilist))
            down = nexts[i]
            count = down.count
            count < fmt.mincount && continue
            count < noisefloor && continue
            str = strs[i]
            isempty(str) && (str = "$count unknown stackframe")
            noisefloor_down = fmt.noisefloor > 0 ? floor(Int, fmt.noisefloor * sqrt(count)) : 0
            pushfirst!(worklist, (down, level + 1, noisefloor_down, str))
        end
    end
end

function tree(io::IO, data::Vector{UInt64}, lidict::Union{LineInfoFlatDict, LineInfoDict}, cols::Int, fmt::ProfileFormat)
    if fmt.combine
        root = tree!(StackFrameTree{StackFrame}(), data, lidict, fmt.C)
    else
        root = tree!(StackFrameTree{UInt64}(), data, lidict, fmt.C)
    end
    if isempty(root.down)
        warning_empty()
        return
    end
    tree(io, root, cols, fmt)
    nothing
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

warning_empty() = @warn """
            There were no samples collected. Run your program longer (perhaps by
            running it multiple times), or adjust the delay between samples with
            `Profile.init()`."""

end # module
