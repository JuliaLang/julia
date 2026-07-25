module Allocs

global print # Allocs.print is separate from both Base.print and Profile.print
public @profile,
    clear,
    print,
    fetch

using ..Profile: Profile, ProfileFormat, StackFrameTree, print_flat, print_tree
using Base.StackTraces: StackTrace, StackFrame, lookup
using Base: InterpreterIP

# --- Raw results structs, originally defined in C ---

# The C jl_bt_element_t object contains either an IP pointer (size_t) or a void*.
const BTElement = Csize_t;

# matches jl_raw_backtrace_t on the C side
struct RawBacktrace
    data::Ptr{BTElement} # in C: *jl_bt_element_t
    size::Csize_t
end

# matches jl_raw_alloc_t on the C side
struct RawAlloc
    type::Ptr{Type}
    backtrace::RawBacktrace
    size::Csize_t
    task::Ptr{Cvoid}
    timestamp::UInt64
end

# matches jl_profile_allocs_raw_results_t on the C side
struct RawResults
    allocs::Ptr{RawAlloc}
    num_allocs::Csize_t
end

"""
    Profile.Allocs.@profile [sample_rate=0.1] expr

Profile allocations that happen during `expr`, returning the result of `expr`. Use `Profile.Allocs.fetch()` to retrieve the AllocResults.

A sample rate of 1.0 will record everything; 0.0 will record nothing.

```julia
julia> Profile.Allocs.@profile sample_rate=0.01 peakflops()
1.03733270279065e11

julia> results = Profile.Allocs.fetch()

julia> last(sort(results.allocs, by=x->x.size))
Profile.Allocs.Alloc(Vector{Any}, Base.StackTraces.StackFrame[_new_array_ at array.c:127, ...], 5576)
```

See the profiling tutorial in the Julia documentation for more information.

!!! compat "Julia 1.11"

    Older versions of Julia could not capture types in all cases. In older versions of
    Julia, if you see an allocation of type `Profile.Allocs.UnknownType`, it means that
    the profiler doesn't know what type of object was allocated. This mainly happened when
    the allocation was coming from generated code produced by the compiler. See
    [issue #43688](https://github.com/JuliaLang/julia/issues/43688) for more info.

    Since Julia 1.11, all allocations should have a type reported.

!!! compat "Julia 1.8"
    The allocation profiler was added in Julia 1.8.
"""
macro profile(opts, ex)
    _prof_expr(ex, opts)
end
macro profile(ex)
    _prof_expr(ex, :(sample_rate=0.1))
end

function _prof_expr(expr, opts)
    quote
        $start(; $(esc(opts)))
        Base.@__tryfinally(
            $(esc(expr))
            ,
            $stop()
        )
    end
end

# serializes the API entry points: e.g. `clear` frees the C-side buffers that a
# concurrent `fetch` would be reading
const allocs_lock = Base.ReentrantLock()

"""
    Profile.Allocs.start(; sample_rate::Real)

Begin recording allocations with the given sample rate
A sample rate of 1.0 will record everything; 0.0 will record nothing.
"""
function start(; sample_rate::Real)
    @lock allocs_lock begin
        ccall(:jl_start_alloc_profile, Cvoid, (Cdouble,), Float64(sample_rate))
    end
end

"""
    Profile.Allocs.stop()

Stop recording allocations.
"""
function stop()
    @lock allocs_lock begin
        ccall(:jl_stop_alloc_profile, Cvoid, ())
    end
end

"""
    Profile.Allocs.clear()

Clear all previously profiled allocation information from memory.
"""
function clear()
    @lock allocs_lock begin
        ccall(:jl_free_alloc_profile, Cvoid, ())
    end
    return nothing
end

"""
    Profile.Allocs.fetch()

Retrieve the recorded allocations, and decode them into Julia
objects which can be analyzed.
"""
function fetch()
    # hold the lock through `decode`, which reads the C-side buffers
    @lock allocs_lock begin
        raw_results = ccall(:jl_fetch_alloc_profile, RawResults, ())
        return decode(raw_results)
    end
end

# decoded results

struct Alloc
    type::Any
    stacktrace::StackTrace
    size::Int
    task::Ptr{Cvoid} # N.B. unrooted, may not be valid
    timestamp::UInt64
end

struct AllocResults
    allocs::Vector{Alloc}
end

# Without this, the Alloc's stacktrace prints for lines and lines and lines...
function Base.show(io::IO, a::Alloc)
    stacktrace_sample = length(a.stacktrace) >= 1 ? "$(a.stacktrace[1]), ..." : ""
    Base.print(io, "$Alloc($(a.type), $StackFrame[$stacktrace_sample], $(a.size))")
end

const BacktraceCache = Dict{BTElement,Vector{StackFrame}}

# copied from julia_internal.h
JL_BUFF_TAG::UInt = ccall(:jl_get_buff_tag, UInt, ())
const JL_GC_UNKNOWN_TYPE_TAG = UInt(0xdeadaa03)

function __init__()
    global JL_BUFF_TAG = ccall(:jl_get_buff_tag, UInt, ())
end

struct CorruptType end
struct BufferType end
struct UnknownType end

# recorded type pointers are marked as GC roots while stored in the profile
# (see `jl_gc_foreach_alloc_profile_root`), so the pointer is safe to load here
function load_type(ptr::Ptr{Type})
    if UInt(ptr) < UInt(4096)
        return CorruptType
    elseif UInt(ptr) == JL_BUFF_TAG
        return BufferType
    elseif UInt(ptr) == JL_GC_UNKNOWN_TYPE_TAG
        return UnknownType
    end
    return unsafe_pointer_to_objref(ptr)
end

function decode(raw_results::RawResults)::AllocResults
    raw_allocs = [unsafe_load(raw_results.allocs, i) for i in 1:raw_results.num_allocs]
    backtraces = [load_backtrace(a.backtrace) for a in raw_allocs]
    # symbol lookup dominates decoding, so do it once per unique ip, in parallel
    # (the same approach as `Profile.getdict!`)
    cache = BacktraceCache()
    unique_ips = unique(Iterators.flatten(backtraces))
    if !isempty(unique_ips)
        sort!(unique_ips) # help each thread to get a disjoint set of libraries, as much as possible
        lookups = Vector{Vector{StackFrame}}(undef, length(unique_ips))
        @sync for part in Iterators.partition(eachindex(unique_ips), div(length(unique_ips), Threads.threadpoolsize(), RoundUp))
            Threads.@spawn for i in part
                lookups[i] = lookup(unique_ips[i])
            end
        end
        for i in eachindex(unique_ips)
            cache[unique_ips[i]] = lookups[i]
        end
    end
    allocs = [
        Alloc(
            load_type(raw_allocs[i].type),
            stacktrace_memoized(cache, backtraces[i]),
            UInt(raw_allocs[i].size),
            raw_allocs[i].task,
            raw_allocs[i].timestamp
        )
        for i in eachindex(raw_allocs)
    ]
    return AllocResults(allocs)
end

function load_backtrace(trace::RawBacktrace)::Vector{BTElement}
    out = Vector{BTElement}()
    n = Int(trace.size)
    i = 1
    while i <= n
        e = unsafe_load(trace.data, i)
        if e == typemax(BTElement) # JL_BT_NON_PTR_ENTRY: start of an extended entry
            # Extended entries (e.g. interpreter frames) hold unrooted object
            # pointers, not native instruction pointers, so they cannot be
            # decoded here; skip over them (size is encoded in the descriptor).
            # Those frames are therefore missing from the reported stack.
            i + 1 <= n || break # truncated entry; nothing more to decode
            descriptor = unsafe_load(trace.data, i + 1)
            ngc = Int(descriptor & 0x7)
            nptr = Int((descriptor >> 3) & 0x7)
            i += 2 + ngc + nptr
            continue
        end
        push!(out, e)
        i += 1
    end

    return out
end

function stacktrace_memoized(
    cache::BacktraceCache,
    trace::Vector{BTElement},
    c_funcs::Bool=true
)::StackTrace
    stack = StackTrace()
    for ip in trace
        frames = get(cache, ip) do
            res = lookup(ip)
            cache[ip] = res
            return res
        end
        for frame in frames
            # Skip frames that come from C calls.
            if c_funcs || !frame.from_c
                push!(stack, frame)
            end
        end
    end
    return stack
end

function warning_empty()
    @warn """
    There were no samples collected.
    Run your program longer (perhaps by running it multiple times),
    or adjust the frequency of samples to record every event with
    the `sample_rate=1.0` kwarg."""
end


"""
    Profile.Allocs.print([io::IO = stdout,] [data::AllocResults = fetch()]; kwargs...)

Prints profiling results to `io` (by default, `stdout`). If you do not
supply a `data` argument, the internal buffer of accumulated backtraces
will be used.

See `Profile.print` for an explanation of the valid keyword arguments; of those,
`format`, `C`, `maxdepth`, `mincount`, `noisefloor`, `sortedby` and `recur` are
supported here.
"""
print(; kwargs...) =
    Profile.print(stdout, fetch(); kwargs...)
print(io::IO; kwargs...) =
    Profile.print(io, fetch(); kwargs...)
print(io::IO, data::AllocResults; kwargs...) =
    Profile.print(io, data; kwargs...)
Profile.print(data::AllocResults; kwargs...) =
    Profile.print(stdout, data; kwargs...)

function Profile.print(io::IO,
        data::AllocResults,
        ;
        format = :tree,
        C = false,
        #combine = true,
        maxdepth::Int = typemax(Int),
        mincount::Int = 0,
        noisefloor = 0,
        sortedby::Symbol = :filefuncline,
        recur::Symbol = :off,
        )
    pf = ProfileFormat(;C, maxdepth, mincount, noisefloor, sortedby, recur)
    Profile.print(io, data, pf, format)
    return
end

function Profile.print(io::IO, data::AllocResults, fmt::ProfileFormat, format::Symbol)
    cols::Int = Base.displaysize(io)[2]
    fmt.recur ∈ (:off, :flat, :flatc) || throw(ArgumentError("recur value not recognized"))
    data = data.allocs
    if format === :tree
        tree(io, data, cols, fmt)
    elseif format === :flat
        fmt.recur === :off || throw(ArgumentError("format flat only implements recur=:off"))
        flat(io, data, cols, fmt)
    else
        throw(ArgumentError("output format $(repr(format)) not recognized"))
    end
    nothing
end


function parse_flat(::Type{T}, data::Vector{Alloc}, C::Bool) where T
    lilist = StackFrame[]
    n = Int[]
    m = Int[]
    lilist_idx = Dict{T, Int}()
    # generation at which each lilist entry was last counted; a per-record
    # generation bump replaces an expensive Set of frames for recursion dedup
    seen_gen = Int[]
    gen = 0
    totalbytes = 0
    for r in data
        first = true
        gen += 1
        nb = r.size # or 1 for counting
        totalbytes += nb
        for frame in r.stacktrace
            !C && frame.from_c && continue
            key = (T === UInt64 ? frame.pointer : frame)
            idx = get!(lilist_idx, key, length(lilist) + 1)
            if idx > length(lilist)
                push!(seen_gen, gen)
                push!(lilist, frame)
                push!(n, nb)
                push!(m, 0)
            elseif seen_gen[idx] != gen
                seen_gen[idx] = gen
                n[idx] += nb
            end
            if first
                m[idx] += nb
                first = false
            end
        end
    end
    @assert length(lilist) == length(n) == length(m) == length(lilist_idx)
    return (lilist, n, m, totalbytes)
end

function flat(io::IO, data::Vector{Alloc}, cols::Int, fmt::ProfileFormat)
    fmt.combine || throw(ArgumentError("combine=false is not supported for allocation profiles"))
    lilist, n, m, totalbytes = parse_flat(fmt.combine ? StackFrame : UInt64, data, fmt.C)
    filenamemap = Profile.FileNameMap()
    if isempty(lilist)
        warning_empty()
        return true
    end
    print_flat(io, lilist, n, m, cols, filenamemap, fmt)
    Base.println(io, "Total snapshots: ", length(data))
    Base.println(io, "Total bytes: ", totalbytes)
    return false
end

function tree!(root::StackFrameTree{T}, all::Vector{Alloc}, C::Bool, recur::Symbol) where {T}
    tops = Vector{StackFrameTree{T}}()
    build = Dict{T, StackFrameTree{T}}()
    for r in all
        first = true
        nb = r.size # or 1 for counting
        root.recur = 0
        root.count += nb
        parent = root
        for i in reverse(eachindex(r.stacktrace))
            frame = r.stacktrace[i]
            key = (T === UInt64 ? frame.pointer : frame)
            if (recur === :flat && !frame.from_c) || recur === :flatc
                # see if this frame already has a parent
                this = get!(build, frame, parent)
                if this !== parent
                    # Rewind the `parent` tree back, if this exact ip (FIXME) was already present *higher* in the current tree
                    push!(tops, parent)
                    parent = this
                end
            end
            !C && frame.from_c && continue
            this = get!(StackFrameTree{T}, parent.down, key)
            if recur === :off || this.recur == 0
                this.frame = frame
                this.up = parent
                this.count += nb
                this.recur = 1
            else
                this.count_recur += 1
            end
            parent = this
        end
        parent.overhead += nb
        if recur !== :off
            # We mark all visited nodes to so we'll only count those branches
            # once for each backtrace. Reset that now for the next backtrace.
            empty!(build)
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
                this.flat_count += nb
                this = this.up
            end
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
    return root
end

function tree(io::IO, data::Vector{Alloc}, cols::Int, fmt::ProfileFormat)
    fmt.combine || throw(ArgumentError("combine=false is not supported for allocation profiles"))
    if fmt.combine
        root = tree!(StackFrameTree{StackFrame}(), data, fmt.C, fmt.recur)
    else
        root = tree!(StackFrameTree{UInt64}(), data, fmt.C, fmt.recur)
    end
    print_tree(io, root, cols, fmt, false)
    if isempty(root.down)
        warning_empty()
        return true
    end
    Base.println(io, "Total snapshots: ", length(data))
    Base.println(io, "Total bytes: ", root.count)
    return false
end

end
