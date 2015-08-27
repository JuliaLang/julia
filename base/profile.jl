# This file is a part of Julia. License is MIT: http://julialang.org/license

module Profile

import Base: hash, ==

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

clear() = ccall(:jl_profile_clear_data, Void, ())

function print{T<:Unsigned}(io::IO, data::Vector{T} = fetch(), lidict::Dict = getdict(data); format = :tree, C = false, combine = true, cols = Base.tty_size()[2], maxdepth::Int = typemax(Int), sortedby::Symbol = :filefuncline)
    if format == :tree
        tree(io, data, lidict, C, combine, cols, maxdepth)
    elseif format == :flat
        flat(io, data, lidict, C, combine, cols, sortedby)
    else
        throw(ArgumentError("output format $(repr(format)) not recognized"))
    end
end
print{T<:Unsigned}(data::Vector{T} = fetch(), lidict::Dict = getdict(data); kwargs...) = print(STDOUT, data, lidict; kwargs...)

function retrieve()
    data = fetch()
    copy(data), getdict(data)
end

function getdict(data::Vector{UInt})
    uip = unique(data)
    Dict{UInt, LineInfo}([ip=>lookup(ip) for ip in uip])
end

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
clear_malloc_data() = ccall(:jl_clear_malloc_data, Void, ())


####
#### Internal interface
####
immutable LineInfo
    func::UTF8String
    file::UTF8String
    line::Int
    inlined_file::ByteString
    inlined_line::Int
    fromC::Bool
    ip::Int64 # large enough that this struct can be losslessly read on any machine (32 or 64 bit)
end

const UNKNOWN = LineInfo("?", "?", -1, "?", -1, true, 0)

#
# If the LineInfo has function and line information, we consider two of them the same
# if they share the same function/line information. For unknown functions, line==ip
# so we never actually need to consider the .ip field.
#
==(a::LineInfo, b::LineInfo) = a.line == b.line && a.fromC == b.fromC && a.func == b.func && a.file == b.file

function hash(li::LineInfo, h::UInt)
    h += 0xf4fbda67fe20ce88 % UInt
    h = hash(li.line, h)
    h = hash(li.file, h)
    h = hash(li.func, h)
end

# C wrappers
start_timer() = ccall(:jl_profile_start_timer, Cint, ())

stop_timer() = ccall(:jl_profile_stop_timer, Void, ())

is_running() = ccall(:jl_profile_is_running, Cint, ())!=0

get_data_pointer() = convert(Ptr{UInt}, ccall(:jl_profile_get_data, Ptr{UInt8}, ()))

len_data() = convert(Int, ccall(:jl_profile_len_data, Csize_t, ()))

maxlen_data() = convert(Int, ccall(:jl_profile_maxlen_data, Csize_t, ()))

function lookup(ip::Ptr{Void})
    info = ccall(:jl_lookup_code_address, Any, (Ptr{Void},Cint), ip, false)
    if length(info) == 7
        return LineInfo(string(info[1]), string(info[2]), Int(info[3]), string(info[4]), Int(info[5]), info[6], Int64(info[7]))
    else
        return UNKNOWN
    end
end
lookup(ip::UInt) = lookup(convert(Ptr{Void},ip))

error_codes = Dict{Int,UTF8String}(
    -1=>"cannot specify signal action for profiling",
    -2=>"cannot create the timer for profiling",
    -3=>"cannot start the timer for profiling",
    -4=>"cannot unblock SIGUSR1")

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
    keep = !Bool[x == UNKNOWN || x.line == 0 || (x.fromC && !C) for x in lilist]
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

function print_flat(io::IO, lilist::Vector{LineInfo}, n::Vector{Int}, combine::Bool, cols::Integer, sortedby)
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
    maxfile = 0
    maxfunc = 0
    for li in lilist
        maxline = max(maxline, li.line)
        maxfile = max(maxfile, length(li.file))
        maxfunc = max(maxfunc, length(li.func))
    end
    wline = max(5, ndigits(maxline))
    ntext = cols - wcounts - wline - 3
    if maxfile+maxfunc <= ntext
        wfile = maxfile
        wfunc = maxfunc
    else
        wfile = floor(Integer,2*ntext/5)
        wfunc = floor(Integer,3*ntext/5)
    end
    println(io, lpad("Count", wcounts, " "), " ", rpad("File", wfile, " "), " ", rpad("Function", wfunc, " "), " ", lpad("Line", wline, " "))
    for i = 1:length(n)
        li = lilist[i]
        println(io, lpad(string(n[i]), wcounts, " "), " ", rpad(truncto(li.file, wfile), wfile, " "), " ", rpad(truncto(li.func, wfunc), wfunc, " "), " ", lpad(string(li.line), wline, " "))
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

tree_format_linewidth(x::LineInfo) = ndigits(x.line)+6

function tree_format(lilist::Vector{LineInfo}, counts::Vector{Int}, level::Int, cols::Integer)
    nindent = min(cols>>1, level)
    ndigcounts = ndigits(maximum(counts))
    ndigline = maximum([tree_format_linewidth(x) for x in lilist])
    ntext = cols-nindent-ndigcounts-ndigline-5
    widthfile = floor(Integer,0.4ntext)
    widthfunc = floor(Integer,0.6ntext)
    strs = Array(UTF8String, length(lilist))
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
            if li.line == li.ip
                strs[i] = string(base,
                          rpad(string(counts[i]), ndigcounts, " "),
                          " ","unknown function (ip: 0x",hex(li.ip,2*sizeof(Ptr{Void})),
                          ")")
            else
                base = string(base,
                              rpad(string(counts[i]), ndigcounts, " "),
                              " ",
                              truncto(string(li.file), widthfile),
                              "; ",
                              truncto(string(li.func), widthfunc),
                              "; ")
                if li.line == -1
                    strs[i] = string(base, "(unknown line)")
                else
                    strs[i] = string(base, "line: ", li.line)
                end
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
        d = Dict{LineInfo,Vector{Int}}()
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
        lilist = Array(LineInfo, dlen)
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
        lilist = Array(LineInfo, dlen)
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
    counts = Dict{LineInfo, Int}()
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
function truncto(str::ByteString, w::Int)
    ret = str;
    if length(str) > w
        ret = string("...", str[end-w+4:end])
    end
    ret
end

# Order alphabetically (file, function) and then by line number
function liperm(lilist::Vector{LineInfo})
    comb = Array(UTF8String, length(lilist))
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
    keep = Bool[d == 0 || lidict[d].fromC == false for d in data]
    data[keep]
end

end # module
