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

Profile allocations that happen during `expr`, returning
both the result and AllocResults struct.

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

"""
    Profile.Allocs.start(sample_rate::Real)

Begin recording allocations with the given sample rate
A sample rate of 1.0 will record everything; 0.0 will record nothing.
"""
function start(; sample_rate::Real)
    ccall(:jl_start_alloc_profile, Cvoid, (Cdouble,), Float64(sample_rate))
end

"""
    Profile.Allocs.stop()

Stop recording allocations.
"""
function stop()
    ccall(:jl_stop_alloc_profile, Cvoid, ())
end

"""
    Profile.Allocs.clear()

Clear all previously profiled allocation information from memory.
"""
function clear()
    ccall(:jl_free_alloc_profile, Cvoid, ())
    return nothing
end

"""
    Profile.Allocs.fetch()

Retrieve the recorded allocations, and decode them into Julia
objects which can be analyzed.
"""
function fetch()
    raw_results = ccall(:jl_fetch_alloc_profile, RawResults, ())
    return decode(raw_results)
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

function decode_alloc(cache::BacktraceCache, raw_alloc::RawAlloc)::Alloc
    Alloc(
        load_type(raw_alloc.type),
        stacktrace_memoized(cache, load_backtrace(raw_alloc.backtrace)),
        UInt(raw_alloc.size),
        raw_alloc.task,
        raw_alloc.timestamp
    )
end

function decode(raw_results::RawResults)::AllocResults
    cache = BacktraceCache()
    allocs = [
        decode_alloc(cache, unsafe_load(raw_results.allocs, i))
        for i in 1:raw_results.num_allocs
    ]
    return AllocResults(allocs)
end

function load_backtrace(trace::RawBacktrace)::Vector{BTElement}
    out = Vector{BTElement}()
    for i in 1:trace.size
        push!(out, unsafe_load(trace.data, i))
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
supply a `data` vector, the internal buffer of accumulated backtraces
will be used.

See `Profile.print` for an explanation of the valid keyword arguments.
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
        groupby::Union{Symbol,AbstractVector{Symbol}} = :none,
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
    recursive = Set{T}()
    totalbytes = 0
    for r in data
        first = true
        empty!(recursive)
        nb = r.size # or 1 for counting
        totalbytes += nb
        for frame in r.stacktrace
            !C && frame.from_c && continue
            key = (T === UInt64 ? ip : frame)
            idx = get!(lilist_idx, key, length(lilist) + 1)
            if idx > length(lilist)
                push!(recursive, key)
                push!(lilist, frame)
                push!(n, nb)
                push!(m, 0)
            elseif !(key in recursive)
                push!(recursive, key)
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
    fmt.combine || error(ArgumentError("combine=false"))
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
            key = (T === UInt64 ? ip : frame)
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
    fmt.combine || error(ArgumentError("combine=false"))
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
