module Time
using Profile.StackTraceTools

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
    init(; n::Integer, delay::Float64)

Configure the `delay` between backtraces (measured in seconds), and the number `n` of
instruction pointers that may be stored. Each instruction pointer corresponds to a single
line of code; backtraces generally consist of a long list of instruction pointers. Default
settings can be obtained by calling this function with no arguments, and each can be set
independently using keywords or in the order `(n, delay)`.
"""
function init(; n::Union{Nothing,Integer} = nothing, delay::Union{Nothing,Float64} = nothing)
    n_cur = ccall(:jl_profile_maxlen_data, Csize_t, ())
    delay_cur = ccall(:jl_profile_delay_nsec, UInt64, ())/10^9
    if n === nothing && delay === nothing
        return Int(n_cur), delay_cur
    end
    nnew = (n === nothing) ? n_cur : n
    delaynew = (delay === nothing) ? delay_cur : delay
    init(nnew, delaynew)
end

function init(n::Integer, delay::Float64)
    status = ccall(:jl_profile_init, Cint, (Csize_t, UInt64), n, round(UInt64,10^9*delay))
    if status == -1
        error("could not allocate space for ", n, " instruction pointers")
    end
end


# C wrappers
start_timer() = ccall(:jl_profile_start_timer, Cint, ())
stop_timer() = ccall(:jl_profile_stop_timer, Cvoid, ())
is_running() = ccall(:jl_profile_is_running, Cint, ()) != 0
did_overflow() = ccall(:jl_profile_overflow, Cint, ()) != 0
get_data_pointer() = convert(Ptr{UInt}, ccall(:jl_profile_get_data, Ptr{UInt8}, ()))
len_data() = convert(Int, ccall(:jl_profile_len_data, Csize_t, ()))
maxlen_data() = convert(Int, ccall(:jl_profile_maxlen_data, Csize_t, ()))

error_codes = Dict(
    -1=>"cannot specify signal action for profiling",
    -2=>"cannot create the timer for profiling",
    -3=>"cannot start the timer for profiling",
    -4=>"cannot unblock SIGUSR1")

"""
    get_portable_data() -> data, lidict

"Exports" profiling results in a portable format, returning the set of all backtraces
(`data`) and a dictionary that maps the (session-specific) instruction pointers in `data` to
`LineInfo` values that store the file name, function name, and line number. This function
allows you to save profiling results for future analysis.
"""
function get_portable_data()
    data = get_data()
    return (copy(data), bt_lookup_dict(data))
end

"""
    get_data() -> data

Returns a reference to the internal buffer of backtraces. Note that subsequent operations,
like [`clear`](@ref), can affect `data` unless you first make a copy. Note that the
values in `data` have meaning only on this machine in the current session, because it
depends on the exact memory addresses used in JIT-compiling. This function is primarily for
internal use; [`get_portable_data`](@ref) may be a better choice for most users.
"""
function get_data()
    if did_overflow()
        @warn """The time profile data buffer overflowed; profiling terminated
                 before your program finished. To profile for longer runs, call
                 `Profile.init()` with a larger buffer and/or larger delay."""
    end
    return unsafe_wrap(Array, get_data_pointer(), (len_data(),))
end

"""
    clear()

Clear any existing backtraces from the internal buffer.
"""
clear() = ccall(:jl_profile_clear_data, Cvoid, ())




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
function print(io::IO, data::Vector{<:Unsigned} = get_data(), lidict::Union{LineInfoDict, LineInfoFlatDict} = bt_lookup_dict(data);
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
previous call to [`get_portable_data`](@ref). Supply the vector `data` of backtraces and
a dictionary `lidict` of line information.

See `Profile.print([io], data)` for an explanation of the valid keyword arguments.
"""
print(data::Vector{<:Unsigned} = get_data(), lidict::Union{LineInfoDict, LineInfoFlatDict} = bt_lookup_dict(data); kwargs...) =
    print(stdout, data, lidict; kwargs...)


end # module Time