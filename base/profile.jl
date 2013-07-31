module Profile

import Base: hash, isequal

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
function init(nsamples::Integer, delay::Float64)
    status = ccall(:profile_init, Cint, (Csize_t, Uint64), nsamples, iround(10^9*delay))
    if status == -1
        error("Could not allocate space for ", nsamples, " profiling samples")
    end
end

clear() = ccall(:profile_clear_data, Void, ())

function print(io::IO = STDOUT, data = fetch(); format = :tree, C = false, combine = true, cols = tty_cols())
    if format == :tree
        tree(io, data, C, combine, cols)
    elseif format == :flat
        flat(io, data, C, combine, cols)
    else
        error("Output format ", format, " not recognized")
    end
end


####
#### Internal interface
####
immutable LineInfo
    func::ASCIIString
    file::ASCIIString
    line::Int
end

const UNKNOWN = LineInfo("?", "?", -1)

isequal(a::LineInfo, b::LineInfo) = a.line == b.line && a.func == b.func && a.file == b.file

hash(li::LineInfo) = bitmix(hash(li.func), bitmix(hash(li.file), hash(li.line)))

# C wrappers
start_timer() = ccall(:profile_start_timer, Cint, ())

stop_timer() = ccall(:profile_stop_timer, Void, ())

get_data_pointer() = convert(Ptr{Uint}, ccall(:profile_get_data, Ptr{Uint8}, ()))

len_data() = convert(Int, ccall(:profile_len_data, Csize_t, ()))

maxlen_data() = convert(Int, ccall(:profile_maxlen_data, Csize_t, ()))

function lookup(ip::Uint, doCframes::Bool)
    info = ccall(:jl_lookup_code_address, Any, (Ptr{Void}, Bool), ip, doCframes)
    if length(info) == 3
        return LineInfo(string(info[1]), string(info[2]), int(info[3]))
    else
        return UNKNOWN
    end
end

error_codes = (Int=>ASCIIString)[
    -1=>"Cannot specify signal action for profiling",
    -2=>"Cannot create the timer for profiling",
    -3=>"Cannot start the timer for profiling"]

function fetch()
    len = len_data()
    maxlen = maxlen_data()
    if (len == maxlen)
        warn("the profile data buffer is full; profiling probably terminated\nbefore your program finished. To profile for longer runs, call Profile.init()\nwith a larger buffer and/or larger delay.")
    end
    pointer_to_array(get_data_pointer(), (len,))
end


# Number of backtrace "steps" that are triggered by taking the backtrace, e.g., inside profile_bt
# May be platform-specific?
const btskip = 2

## Print as a flat list
# Counts the number of times each line appears, at any nesting level
function parse_flat(data::Vector{Uint}, doCframes::Bool)
    linecount = (Uint=>Int)[]
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
    buf = Array(Uint, 0)
    n = Array(Int, 0)
    for (k,v) in linecount
        push!(buf, k)
        push!(n, v)
    end
    # Convert instruction pointers to names & line numbers
    lilist = Array(LineInfo, length(buf))
    for i = 1:length(buf)
        lilist[i] = lookup(buf[i], doCframes)
    end
    # Keep only the interpretable ones
    # The ones with no line number might appear multiple times in a single
    # backtrace, giving the wrong impression about the total number of backtraces.
    # Delete them too.
    keep = !Bool[x == UNKNOWN || x.line == 0 for x in lilist]
    n = n[keep]
    lilist = lilist[keep]
    lilist, n
end

function flat(io::IO, data::Vector{Uint}, doCframes::Bool, combine::Bool, cols::Integer)
    lilist, n = parse_flat(data, doCframes)
    if isempty(n)
        warning_empty()
        return
    end
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
    wcounts = max(6, ndigits(max(n)))
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
        wfile = ifloor(2*ntext/5)
        wfunc = ifloor(3*ntext/5)
    end
    println(io, lpad("Count", wcounts, " "), " ", rpad("File", wfile, " "), " ", rpad("Function", wfunc, " "), " ", lpad("Line", wline, " "))
    for i = 1:length(n)
        li = lilist[i]
        println(io, lpad(string(n[i]), wcounts, " "), " ", rpad(truncto(li.file, wfile), wfile, " "), " ", rpad(truncto(li.func, wfunc), wfunc, " "), " ", lpad(string(li.line), wline, " "))
    end
end

## A tree representation
# Identify and counts repetitions of all unique backtraces
function tree_aggregate(data::Array{Uint})
    iz = find(data .== 0)  # find the breaks between backtraces
    treecount = (Vector{Uint}=>Int)[]
    istart = 1+btskip
    for iend in iz
        tmp = data[iend-1:-1:istart]
        treecount[tmp] = get(treecount, tmp, 0)+1
        istart = iend+1+btskip
    end
    bt = Array(Vector{Uint}, 0)
    counts = Array(Int, 0)
    for (k,v) in treecount
        push!(bt, k)
        push!(counts, v)
    end
    bt, counts
end

tree_format_linewidth(x::LineInfo) = ndigits(x.line)+6

function tree_format(lilist::Vector{LineInfo}, counts::Vector{Int}, level::Int, cols::Integer)
    nindent = min(ifloor(cols/2), level)
    ndigcounts = ndigits(max(counts))
    ndigline = max([tree_format_linewidth(x) for x in lilist])
    ntext = cols-nindent-ndigcounts-ndigline-5
    widthfile = ifloor(0.4ntext)
    widthfunc = ifloor(0.6ntext)
    strs = Array(ASCIIString, length(lilist))
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
            base = string(base,
                          rpad(string(counts[i]), ndigcounts, " "),
                          " ",
                          truncto(string(li.file), widthfile),
                          "; ",
                          truncto(string(li.func), widthfunc),
                          "; ")
            strs[i] = string(base, "line: ", li.line)
        else
            strs[i] = ""
        end
    end
    strs
end

# Print a "branch" starting at a particular level. This gets called recursively.
function tree(io::IO, bt::Vector{Vector{Uint}}, counts::Vector{Int}, lidict::Dict, level::Int, combine::Bool, cols::Integer)
    # Organize backtraces into groups that are identical up to this level
    if combine
        # Combine based on the line information
        d = (LineInfo=>Vector{Int})[]
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
        d = (Uint=>Vector{Int})[]
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
            tree(io, bt[idx], counts[idx], lidict, level+1, combine, cols)
        end
    end
end

function tree(io::IO, data::Vector{Uint}, doCframes::Bool, combine::Bool, cols::Integer)
    bt, counts = tree_aggregate(data)
    if isempty(counts)
        warning_empty()
        return
    end
    uip = unique(data)
    lidict = Dict(uip, [lookup(ip, doCframes) for ip in uip])
    level = 0
    len = Int[length(x) for x in bt]
    keep = len .> 0
    tree(io, bt[keep], counts[keep], lidict, level, combine, cols)
end

# Utilities
function truncto(str::ASCIIString, w::Int)
    ret = str;
    if length(str) > w
        ret = string("...", str[end-w+4:end])
    end
    ret
end

# Order alphabetically (file, function) and then by line number
function liperm(lilist::Vector{LineInfo})
    comb = Array(ASCIIString, length(lilist))
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


end # module
