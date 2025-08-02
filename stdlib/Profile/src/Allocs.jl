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

"""
    RawBacktrace

Represent the raw backtrace information as it is captured at the C level within Julia. 
It holds a pointer to the raw backtrace data (`data`) and the size of this data (`size`). 
This struct mirrors the internal C representation used by Julia to manage backtraces, 
facilitating the transfer of data between Julia and its underlying C implementation 
for memory allocation profiling.

# Fields
- `data::Ptr{BTElement}`: A pointer to the beginning of the backtrace data array.
- `size::Csize_t`: The number of elements in the backtrace data array.
"""
struct RawBacktrace
    data::Ptr{BTElement} # in C: *jl_bt_element_t
    size::Csize_t
end

"""
    RawAlloc

Represent a single memory allocation event's raw data, as captured at the C level within Julia. 
It includes information about the type of the allocated object, the backtrace associated with the allocation event, 
the size of the allocation, the task in which the allocation occurred, and a timestamp marking the allocation time. 
This struct is designed to closely mirror the corresponding C structure used internally by Julia, 
making it easier to process allocation data in Julia's environment.

# Fields
- `type::Ptr{Type}`: A pointer to the type of the allocated object.
- `backtrace::RawBacktrace`: The backtrace at the point of allocation.
- `size::Csize_t`: The size of the allocation, in bytes.
- `task::Ptr{Cvoid}`: A pointer to the task in which the allocation occurred.
- `timestamp::UInt64`: A timestamp (in an unspecified unit) marking when the allocation occurred.
"""
struct RawAlloc
    type::Ptr{Type}
    backtrace::RawBacktrace
    size::Csize_t
    task::Ptr{Cvoid}
    timestamp::UInt64
end

"""
    RawResults

Serve as the direct result of a profiling session. 
This struct aggregates the raw data that has been collected, facilitating subsequent processing 
and analysis to decode and understand the memory allocation behavior of the program.

# Fields
- `allocs::Ptr{RawAlloc}`: Pointer to the array of `RawAlloc` records, each representing a single memory allocation event.
- `num_allocs::Csize_t`: The total number of allocation events recorded, indicating the size of the `allocs` array.
"""

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

"""
    _prof_expr(expr, opts)

Internal helper function for the `@profile` macro. It constructs an expression that starts the allocation profiling, 
executes the given expression `expr`, and then stops the profiling, 
ensuring that profiling is properly terminated even if the executed expression throws an error.

# Arguments
- `expr`: The Julia expression to be profiled.
- `opts`: Options for the profiling session, such as the sampling rate.

# Returns
- A Julia expression that, when evaluated, performs the profiling of `expr` according to `opts`.
"""

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

"""
    Alloc

A high-level representation of a memory allocation event in Julia, decoded from the raw data captured during profiling. 
It provides a user-friendly view of each allocation, including the type of the allocated object (if known), 
a stack trace leading to the allocation, the size of the allocation, the task identifier, and a timestamp of the allocation event. 
This struct is used for analyzing and reporting memory allocation patterns in a more accessible format.

# Fields
- `type::Any`: The type of the allocated object. Can be a specific Julia type or a placeholder for unknown types.
- `stacktrace::StackTrace`: A stack trace object representing the call stack leading to the allocation.
- `size::Int`: The size of the allocated memory, in bytes.
- `task::Ptr{Cvoid}`: A pointer to the task in which the allocation occurred. Note that this is unrooted and may not always be valid.
- `timestamp::UInt64`: A timestamp marking when the allocation occurred.
"""


struct Alloc
    type::Any
    stacktrace::StackTrace
    size::Int
    task::Ptr{Cvoid} # N.B. unrooted, may not be valid
    timestamp::UInt64
end

"""
    AllocResults

Represent the results of a memory allocation profiling session. 
This struct makes it easier to manage and analyze the set of all allocations recorded during a profiling run, 
providing a convenient way to access and iterate over individual allocation events.

# Fields
- `allocs::Vector{Alloc}`: A vector of `Alloc` instances, each representing a single memory allocation event.
"""

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


"""
    __init__()

Initialization function for the `Allocs` module. 
It is automatically called when the module is loaded, setting up necessary global constants and state for the module's operation. 
Specifically, it initializes the `JL_BUFF_TAG` global variable used in memory allocation profiling.

"""

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


"""
    decode_alloc(cache::BacktraceCache, raw_alloc::RawAlloc)::Alloc

Decodes a single `RawAlloc` struct into a high-level `Alloc` struct, 
making the raw allocation data more accessible and interpretable. It uses a cache to efficiently lookup and memoize stack frames.

# Arguments
- `cache`: A cache for memoizing stack frame lookups to improve performance.
- `raw_alloc`: A `RawAlloc` instance representing the raw allocation data to be decoded.

# Returns
- An `Alloc` instance representing the decoded allocation event.
"""

function decode_alloc(cache::BacktraceCache, raw_alloc::RawAlloc)::Alloc
    Alloc(
        load_type(raw_alloc.type),
        stacktrace_memoized(cache, load_backtrace(raw_alloc.backtrace)),
        UInt(raw_alloc.size),
        raw_alloc.task,
        raw_alloc.timestamp
    )
end

"""
    decode(raw_results::RawResults)::AllocResults

Decodes the raw results of a memory allocation profiling session (`RawResults`) 
into a more interpretable format (`AllocResults`). This function processes an array of 
`RawAlloc` instances, converting them into a vector of `Alloc` instances.

# Arguments
- `raw_results`: The raw profiling data to be decoded.

# Returns
- An `AllocResults` instance containing a vector of decoded `Alloc` instances.
"""

function decode(raw_results::RawResults)::AllocResults
    cache = BacktraceCache()
    allocs = [
        decode_alloc(cache, unsafe_load(raw_results.allocs, i))
        for i in 1:raw_results.num_allocs
    ]
    return AllocResults(allocs)
end

"""
    load_backtrace(trace::RawBacktrace)::Vector{BTElement}

Loads the backtrace data from a `RawBacktrace` struct into a vector of `BTElement`, representing the call stack at the point of allocation.

# Arguments
- `trace`: A `RawBacktrace` instance containing the raw backtrace data.

# Returns
- A vector of `BTElement` representing the loaded backtrace.
"""


function load_backtrace(trace::RawBacktrace)::Vector{BTElement}
    out = Vector{BTElement}()
    for i in 1:trace.size
        push!(out, unsafe_load(trace.data, i))
    end

    return out
end


"""
    stacktrace_memoized(cache::BacktraceCache, trace::Vector{BTElement}, c_funcs::Bool=true)::StackTrace

Generates a stack trace from a vector of backtrace elements (`BTElement`), 
using a cache to memoize and efficiently retrieve stack frames. It can optionally filter out C function calls from the stack trace.

# Arguments
- `cache`: A cache for memoizing stack frame lookups.
- `trace`: A vector of `BTElement` representing the backtrace to be processed.
- `c_funcs`: A boolean flag indicating whether C function calls should be included in the stack trace.

# Returns
- A `StackTrace` object representing the processed stack trace, suitable for analysis and display.
"""


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


"""
    warning_empty()

Generates a warning message indicating that no samples were collected during the profiling session. 
This function should be called when analysis functions detect an empty dataset, suggesting to the user to 
run the profiling session longer or adjust the sampling rate.
"""


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
