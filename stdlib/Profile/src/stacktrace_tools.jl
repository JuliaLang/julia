module StackTraceTools
import Base.StackTraces: lookup, UNKNOWN, show_spec_linfo, StackFrame

export LineInfoDict, LineInfoFlatDict, ProfileFormat
export bt_lookup_dict, flatten, callers, tree, flat


const LineInfoDict = Dict{Union{UInt,Base.InterpreterIP}, Vector{StackFrame}}
const LineInfoFlatDict = Dict{Union{UInt,Base.InterpreterIP}, StackFrame}

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
    bt_lookup_dict(data::Vector{UInt})

Given a list of instruction pointers, build a dictionary mapping from those pointers to
LineInfo objects (as returned by `StackTraces.lookup()`).  This allows us to quickly take
a list of instruction pointers and convert it to `LineInfo` objects.
"""
function bt_lookup_dict(data)
    return LineInfoDict(x=>lookup(x) for x in unique(data))
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
backtrace `data` obtained from [`get_portable_data`](@ref); otherwise, the current internal
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
callers(funcname::String; kwargs...) = callers(funcname, get_portable_data()...; kwargs...)
callers(func::Function, bt::Vector, lidict::LineInfoFlatDict; kwargs...) =
    callers(string(func), bt, lidict; kwargs...)
callers(func::Function; kwargs...) = callers(string(func), get_portable_data()...; kwargs...)


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
                down = parent.down
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
        empty!(node.builder_key)
        empty!(node.builder_value)
        foreach(cleanup!, values(node.down))
        nothing
    end
    cleanup!(root)
    return root
end

# Print a "branch" starting at a particular level. This gets called recursively.
function tree(io::IO, bt::StackFrameTree, level::Int, cols::Int, fmt::ProfileFormat, noisefloor::Int)
    level > fmt.maxdepth && return
    isempty(bt.down) && return
    # Order the line information
    nexts = collect(values(bt.down))
    lilist = collect(frame.frame for frame in nexts)
    counts = collect(frame.count for frame in nexts)
    # Generate the string for each line
    strs = tree_format(lilist, counts, level, cols)
    # Recurse to the next level
    for i in liperm(lilist)
        down = nexts[i]
        count = down.count
        count < fmt.mincount && continue
        count < noisefloor && continue
        str = strs[i]
        println(io, isempty(str) ? "$count unknown stackframe" : str)
        noisefloor_down = fmt.noisefloor > 0 ? floor(Int, fmt.noisefloor * sqrt(count)) : 0
        tree(io, down, level + 1, cols, fmt, noisefloor_down)
    end
    nothing
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
    level = 0
    tree(io, root, level, cols, fmt, 0)
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

end # module StackTraceTools
