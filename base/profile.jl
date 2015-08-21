# This file is a part of Julia. License is MIT: http://julialang.org/license

module Profile

import Base.StackTraces: lookup, UNKNOWN, show_spec_linfo

export @profile

macro profile(ex)
    quote
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
function init(; n::Union{Void,Integer} = nothing, delay::Union{Void,Float64} = nothing)
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

# init with default values
# Use a max size of 1M profile samples, and fire timer every 1ms
@windows? (__init__() = init(1_000_000, 0.01)) : (__init__() = init(1_000_000, 0.001))

"""
    clear()

Clear any existing backtraces from the internal buffer.
"""
clear() = ccall(:jl_profile_clear_data, Void, ())

"""
    print([io::IO = STDOUT,] [data::Vector]; format = :tree, C = false, combine = true, maxdepth = typemax(Int), sortedby = :filefuncline)

Prints profiling results to `io` (by default, `STDOUT`). If you do not
supply a `data` vector, the internal buffer of accumulated backtraces
will be used. `format` can be `:tree` or `:flat`. If `C==true`,
backtraces from C and Fortran code are shown. `combine==true` merges
instruction pointers that correspond to the same line of
code. `maxdepth` can be used to limit the depth of printing in `:tree`
format, while `sortedby` can be used to control the order in `:flat`
format (`:filefuncline` sorts by the source line, whereas `:count`
sorts in order of number of collected samples).
"""
function print{T<:Unsigned}(io::IO, data::Vector{T} = fetch(), lidict::Dict = getdict(data);
        format = :tree,
        C = false,
        combine = true,
        maxdepth::Int = typemax(Int),
        sortedby::Symbol = :filefuncline)
    cols = Base.displaysize(io)[2]
    if format == :tree
        tree(io, data, lidict, C, combine, cols, maxdepth)
    elseif format == :flat
        flat(io, data, lidict, C, combine, cols, sortedby)
    else
        throw(ArgumentError("output format $(repr(format)) not recognized"))
    end
end

"""
    print([io::IO = STDOUT,] data::Vector, lidict::Dict; kwargs)

Prints profiling results to `io`. This variant is used to examine results exported by a
previous call to [`retrieve`](:func:`retrieve`). Supply the vector `data` of backtraces and
a dictionary `lidict` of line information.

See `Profile.print([io], data)` for an explanation of the valid keyword arguments.
"""
print{T<:Unsigned}(data::Vector{T} = fetch(), lidict::Dict = getdict(data); kwargs...) = print(STDOUT, data, lidict; kwargs...)

"""
    retrieve() -> data, lidict

"Exports" profiling results in a portable format, returning the set of all backtraces
(`data`) and a dictionary that maps the (session-specific) instruction pointers in `data` to
`LineInfo` values that store the file name, function name, and line number. This function
allows you to save profiling results for future analysis.
"""
function retrieve()
    data = fetch()
    copy(data), getdict(data)
end

function getdict(data::Vector{UInt})
    uip = unique(data)
    Dict{UInt, StackFrame}([ip=>lookup(ip) for ip in uip])
end

"""
    callers(funcname, [data, lidict], [filename=<filename>], [linerange=<start:stop>]) -> Vector{Tuple{count, lineinfo}}

Given a previous profiling run, determine who called a particular function. Supplying the
filename (and optionally, range of line numbers over which the function is defined) allows
you to disambiguate an overloaded method. The returned value is a vector containing a count
of the number of calls and line information about the caller. One can optionally supply
backtrace `data` obtained from [`retrieve`](:func:`retrieve`); otherwise, the current internal
profile buffer is used.
"""
function callers end

function callers(funcname::ByteString, bt::Vector{UInt}, lidict; filename = nothing, linerange = nothing)
    if filename === nothing && linerange === nothing
        return callersf(li -> li.func == funcname, bt, lidict)
    end
    filename === nothing && throw(ArgumentError("if supplying linerange, you must also supply the filename"))
    if linerange === nothing
        return callersf(li -> li.func == funcname && li.file == filename, bt, lidict)
    else
        return callersf(li -> li.func == funcname && li.file == filename && in(li.line, linerange), bt, lidict)
    end
end

callers(funcname::ByteString; kwargs...) = callers(funcname, retrieve()...; kwargs...)
callers(func::Function, bt::Vector{UInt}, lidict; kwargs...) = callers(string(func), bt, lidict; kwargs...)
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
[`clear_malloc_data`](:func:`clear_malloc_data`). Then execute your command(s) again, quit
Julia, and examine the resulting `*.mem` files.
"""
clear_malloc_data() = ccall(:jl_clear_malloc_data, Void, ())

# C wrappers
start_timer() = ccall(:jl_profile_start_timer, Cint, ())

stop_timer() = ccall(:jl_profile_stop_timer, Void, ())

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
like [`clear`](:func:`clear`), can affect `data` unless you first make a copy. Note that the
values in `data` have meaning only on this machine in the current session, because it
depends on the exact memory addresses used in JIT-compiling. This function is primarily for
internal use; [`retrieve`](:func:`retrieve`) may be a better choice for most users.
"""
function fetch()
    len = len_data()
    maxlen = maxlen_data()
    if (len == maxlen)
        warn("The profile data buffer is full; profiling probably terminated\nbefore your program finished. To profile for longer runs, call Profile.init\nwith a larger buffer and/or larger delay.")
    end
    pointer_to_array(get_data_pointer(), (len,))
end


# Number of backtrace "steps" that are triggered by taking the backtrace, e.g., inside profile_bt
# May be platform-specific?
#@unix_only const btskip = 2
#@windows_only const btskip = 0
const btskip = 0

## Print as a flat list
# Counts the number of times each line appears, at any nesting level
function count_flat{T<:Unsigned}(data::Vector{T})
    linecount = Dict{T,Int}()
    toskip = btskip
    for ip in data
        if toskip > 0
            toskip -= 1
            continue
        end
        if ip == 0
            toskip = btskip
            continue
        end
        linecount[ip] = get(linecount, ip, 0)+1
    end
    iplist = Array(T, 0)
    n = Array(Int, 0)
    for (k,v) in linecount
        push!(iplist, k)
        push!(n, v)
    end
    return iplist, n
end

function parse_flat(iplist, n, lidict, C::Bool)
    # Convert instruction pointers to names & line numbers
    lilist = [lidict[ip] for ip in iplist]
    # Keep only the interpretable ones
    # The ones with no line number might appear multiple times in a single
    # backtrace, giving the wrong impression about the total number of backtraces.
    # Delete them too.
    keep = !Bool[x == UNKNOWN || x.line == 0 || (x.from_c && !C) for x in lilist]
    n = n[keep]
    lilist = lilist[keep]
    lilist, n
end

function flat{T<:Unsigned}(io::IO, data::Vector{T}, lidict::Dict, C::Bool, combine::Bool, cols::Integer, sortedby)
    if !C
        data = purgeC(data, lidict)
    end
    iplist, n = count_flat(data)
    if isempty(n)
        warning_empty()
        return
    end
    lilist, n = parse_flat(iplist, n, lidict, C)
    print_flat(io, lilist, n, combine, cols, sortedby)
end

function print_flat(io::IO, lilist::Vector{StackFrame}, n::Vector{Int}, combine::Bool, cols::Integer, sortedby)
    p = liperm(lilist)
    lilist = lilist[p]
    n = n[p]
    if combine
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
    if sortedby == :count
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
        wfile = floor(Integer,2*ntext/5)
        wfunc = floor(Integer,3*ntext/5)
    end
    println(io, lpad("Count", wcounts, " "), " ", rpad("File", wfile, " "), " ", lpad("Line", wline, " "), " ", rpad("Function", wfunc, " "))
    for i = 1:length(n)
        li = lilist[i]
        Base.print(io, lpad(string(n[i]), wcounts, " "), " ")
        Base.print(io, rpad(rtruncto(string(li.file), wfile), wfile, " "), " ")
        Base.print(io, lpad(string(li.line), wline, " "), " ")
        fname = string(li.func)
        if !li.from_c && !isnull(li.outer_linfo)
            fname = sprint(show_spec_linfo, li)
        end
        Base.print(io, rpad(ltruncto(fname, wfunc), wfunc, " "))
        println(io)
    end
end

## A tree representation
# Identify and counts repetitions of all unique backtraces
function tree_aggregate{T<:Unsigned}(data::Vector{T})
    iz = find(data .== 0)  # find the breaks between backtraces
    treecount = Dict{Vector{T},Int}()
    istart = 1+btskip
    for iend in iz
        tmp = data[iend-1:-1:istart]
        treecount[tmp] = get(treecount, tmp, 0)+1
        istart = iend+1+btskip
    end
    bt = Array(Vector{T}, 0)
    counts = Array(Int, 0)
    for (k,v) in treecount
        if !isempty(k)
            push!(bt, k)
            push!(counts, v)
        end
    end
    bt, counts
end

tree_format_linewidth(x::StackFrame) = ndigits(x.line)+6

function tree_format(lilist::Vector{StackFrame}, counts::Vector{Int}, level::Int, cols::Integer)
    nindent = min(cols>>1, level)
    ndigcounts = ndigits(maximum(counts))
    ndigline = maximum([tree_format_linewidth(x) for x in lilist])
    ntext = cols-nindent-ndigcounts-ndigline-5
    widthfile = floor(Integer,0.4ntext)
    widthfunc = floor(Integer,0.6ntext)
    strs = Array(ByteString, length(lilist))
    showextra = false
    if level > nindent
        nextra = level-nindent
        nindent -= ndigits(nextra)+2
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
                          hex(li.pointer,2*sizeof(Ptr{Void})),
                          ")")
            else
                fname = string(li.func)
                if !li.from_c && !isnull(li.outer_linfo)
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
    strs
end

# Print a "branch" starting at a particular level. This gets called recursively.
function tree{T<:Unsigned}(io::IO, bt::Vector{Vector{T}}, counts::Vector{Int}, lidict::Dict, level::Int, combine::Bool, cols::Integer, maxdepth)
    if level > maxdepth
        return
    end
    # Organize backtraces into groups that are identical up to this level
    if combine
        # Combine based on the line information
        d = Dict{StackFrame,Vector{Int}}()
        for i = 1:length(bt)
            ip = bt[i][level+1]
            key = lidict[ip]
            indx = Base.ht_keyindex(d, key)
            if indx == -1
                d[key] = [i]
            else
                push!(d.vals[indx], i)
            end
        end
        # Generate counts
        dlen = length(d)
        lilist = Array(StackFrame, dlen)
        group = Array(Vector{Int}, dlen)
        n = Array(Int, dlen)
        i = 1
        for (key, v) in d
            lilist[i] = key
            group[i] = v
            n[i] = sum(counts[v])
            i += 1
        end
    else
        # Combine based on the instruction pointer
        d = Dict{T,Vector{Int}}()
        for i = 1:length(bt)
            key = bt[i][level+1]
            indx = Base.ht_keyindex(d, key)
            if indx == -1
                d[key] = [i]
            else
                push!(d.vals[indx], i)
            end
        end
        # Generate counts, and do the code lookup
        dlen = length(d)
        lilist = Array(StackFrame, dlen)
        group = Array(Vector{Int}, dlen)
        n = Array(Int, dlen)
        i = 1
        for (key, v) in d
            lilist[i] = lidict[key]
            group[i] = v
            n[i] = sum(counts[v])
            i += 1
        end
    end
    # Order the line information
    if length(lilist) > 1
        p = liperm(lilist)
        lilist = lilist[p]
        group = group[p]
        n = n[p]
    end
    # Generate the string for each line
    strs = tree_format(lilist, n, level, cols)
    # Recurse to the next level
    len = Int[length(x) for x in bt]
    for i = 1:length(lilist)
        if !isempty(strs[i])
            println(io, strs[i])
        end
        idx = group[i]
        keep = len[idx] .> level+1
        if any(keep)
            idx = idx[keep]
            tree(io, bt[idx], counts[idx], lidict, level+1, combine, cols, maxdepth)
        end
    end
end

function tree{T<:Unsigned}(io::IO, data::Vector{T}, lidict::Dict, C::Bool, combine::Bool, cols::Integer, maxdepth)
    if !C
        data = purgeC(data, lidict)
    end
    bt, counts = tree_aggregate(data)
    if isempty(counts)
        warning_empty()
        return
    end
    level = 0
    len = Int[length(x) for x in bt]
    keep = len .> 0
    tree(io, bt[keep], counts[keep], lidict, level, combine, cols, maxdepth)
end

function callersf(matchfunc::Function, bt::Vector{UInt}, lidict)
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
    [(v[i], k[i]) for i in p]
end

# Utilities
function rtruncto(str::ByteString, w::Int)
    ret = str;
    if length(str) > w
        ret = string("...", str[end-w+4:end])
    end
    ret
end
function ltruncto(str::ByteString, w::Int)
    ret = str;
    if length(str) > w
        ret = string(str[1:w-4], "...")
    end
    ret
end


truncto(str::Symbol, w::Int) = truncto(string(str), w)

# Order alphabetically (file, function) and then by line number
function liperm(lilist::Vector{StackFrame})
    comb = Array(ByteString, length(lilist))
    for i = 1:length(lilist)
        li = lilist[i]
        if li != UNKNOWN
            comb[i] = @sprintf("%s:%s:%06d", li.file, li.func, li.line)
        else
            comb[i] = "zzz"
        end
    end
    sortperm(comb)
end

warning_empty() = warn("""
            There were no samples collected. Run your program longer (perhaps by
            running it multiple times), or adjust the delay between samples with
            Profile.init().""")

function purgeC(data, lidict)
    keep = Bool[d == 0 || lidict[d].from_c == false for d in data]
    data[keep]
end

end # module
