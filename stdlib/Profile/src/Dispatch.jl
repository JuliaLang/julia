# This file is a part of Julia. License is MIT: https://julialang.org/license

module Dispatch

global print # Dispatch.print is separate from both Base.print and Profile.print
public @profile,
    start,
    stop,
    clear,
    fetch,
    print

using ..Profile: ProfileFormat, StackFrameTree, print_tree, short_path, FileNameMap, PACKAGE_FIXEDCOLORS, editor_link
using StyledStrings: @styled_str
using ..Allocs: Allocs, BTElement, RawBacktrace, BacktraceCache, load_backtrace!, stacktrace_memoized
using Base.StackTraces: StackTrace, StackFrame, lookup
using Core: MethodInstance

# matches jl_raw_dispatch_t on the C side
struct RawDispatch
    mi::Ptr{Cvoid}
    backtrace::RawBacktrace
    task::Ptr{Cvoid}
    path::UInt8
end

# matches jl_dispatch_profile_raw_results_t on the C side
struct RawResults
    dispatches::Ptr{RawDispatch}
    num_dispatches::Csize_t
end

"""
    Profile.Dispatch.@profile [sample_rate=0.01] [maxdepth=typemax(Int)] expr

Profile the dynamic dispatches that happen during `expr`, returning the result of `expr`.
A dynamic dispatch is a call whose target method is looked up at run time, because the
types of the arguments were not known when the caller was compiled.

Each dispatch is recorded with probability `sample_rate`, together with a backtrace and
the method specialization that was called. Use [`Profile.Dispatch.print`](@ref) for a
report of the call sites that dispatch the most and [`Profile.Dispatch.fetch`](@ref)
for the recorded samples.

```julia
julia> Profile.Dispatch.@profile sum(Any[1, 2.0, 3]);

julia> Profile.Dispatch.print()
```
"""
macro profile(args...)
    isempty(args) && throw(ArgumentError("`@profile` needs an expression to profile"))
    opts = map(esc, args[1:(end - 1)])
    return quote
        $start(; $(opts...))
        Base.@__tryfinally(
            $(esc(args[end])),
            $stop()
        )
    end
end

const dispatch_lock = Base.ReentrantLock()
# the rate of the samples in the buffer, `nothing` when it holds samples of several rates
sample_rate::Union{Nothing, Float64} = nothing
has_samples::Bool = false
# the rate being recorded at, `nothing` when stopped
active_rate::Union{Nothing, Float64} = nothing

"""
    Profile.Dispatch.start(; sample_rate::Real = 0.01, maxdepth::Integer = typemax(Int))

Begin recording dynamic dispatches, each with probability `sample_rate`.

Taking the backtrace is most of the cost of a sample. `maxdepth` limits it to that many
of the innermost stack frames, which is enough for [`Profile.Dispatch.print`](@ref) when
it is a handful: the report only uses the innermost Julia frame.
"""
function start(; sample_rate::Real = 0.01, maxdepth::Integer = typemax(Int))
    rate = clamp(Float64(sample_rate), 0.0, 1.0)
    @lock dispatch_lock begin
        global sample_rate = (!has_samples || sample_rate == rate) ? rate : nothing
        global has_samples, active_rate = true, rate
        # one more for the frame of the dispatch itself
        ccall(:jl_start_dispatch_profile, Cvoid, (Cdouble, Csize_t), rate, min(max(maxdepth, 1), typemax(Int) - 1) + 1)
    end
    return
end

"""
    Profile.Dispatch.stop()

Stop recording dynamic dispatches.
"""
function stop()
    @lock dispatch_lock begin
        ccall(:jl_stop_dispatch_profile, Cvoid, ())
        global active_rate = nothing
    end
    return
end

"""
    Profile.Dispatch.clear()

Clear the recorded dynamic dispatches from memory.
"""
function clear()
    @lock dispatch_lock begin
        ccall(:jl_free_dispatch_profile, Cvoid, ())
        # recording goes on if it was on: what comes next has the active rate
        global has_samples, sample_rate = active_rate !== nothing, active_rate
    end
    return
end

"""
    DispatchSample

A recorded dynamic dispatch: the `callee` that was called, the `stacktrace` of the call
(innermost frame first, starting at the caller) and how the callee was found:

- `:callsite`: in the small cache indexed by the call site, the fast path;
- `:cache`: in the method cache, after missing the call-site cache;
- `:lookup`: by a full method lookup, which may also have compiled the callee.
"""
struct DispatchSample
    callee::MethodInstance
    stacktrace::StackTrace
    path::Symbol
    task::Ptr{Cvoid} # N.B. unrooted, may not be valid
end

struct DispatchResults
    dispatches::Vector{DispatchSample}
    sample_rate::Union{Nothing, Float64}
end

function Base.show(io::IO, d::DispatchSample)
    Base.print(io, DispatchSample, "(")
    show_callee(io, d.callee)
    Base.print(io, " from ", isempty(d.stacktrace) ? "?" : d.stacktrace[1], ", ", repr(d.path), ")")
end

Base.show(io::IO, r::DispatchResults) =
    Base.print(io, DispatchResults, "(", length(r.dispatches), " samples, sample_rate = ", something(r.sample_rate, "mixed"), ")")

"""
    Profile.Dispatch.fetch()

Retrieve the recorded dynamic dispatches as a `DispatchResults`, whose `dispatches`
field holds one [`DispatchSample`](@ref Profile.Dispatch.DispatchSample) per sample.
"""
function fetch()
    @lock dispatch_lock begin
        raw = ccall(:jl_fetch_dispatch_profile, RawResults, ())
        return DispatchResults(decode(raw), sample_rate)
    end
end

const PATHS = (:callsite, :cache, :lookup)

# frames of the dispatch itself, at the start of every recorded backtrace
is_dispatch_frame(frame::StackFrame) = frame.from_c &&
    frame.func in (:jl_apply_generic, :ijl_apply_generic, :jl_lookup_generic_slow, :jl_lookup_generic_)

function decode(raw::RawResults)
    n = Int(raw.num_dispatches)
    cache = BacktraceCache()
    trace = Vector{BTElement}()
    unique_ips = Vector{BTElement}()
    let seen = Set{BTElement}()
        for i in 1:n
            load_backtrace!(empty!(trace), unsafe_load(raw.dispatches, i).backtrace)
            for ip in trace
                ip in seen || (push!(seen, ip); push!(unique_ips, ip))
            end
        end
    end
    if !isempty(unique_ips)
        # symbol lookup dominates, so do it once per unique ip, in parallel
        sort!(unique_ips)
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
    dispatches = Vector{DispatchSample}(undef, n)
    for i in 1:n
        r = unsafe_load(raw.dispatches, i)
        load_backtrace!(empty!(trace), r.backtrace)
        stack = stacktrace_memoized(cache, trace)
        first = something(findfirst(!is_dispatch_frame, stack), length(stack) + 1)
        deleteat!(stack, 1:(first - 1))
        callee = unsafe_pointer_to_objref(r.mi)::MethodInstance
        dispatches[i] = DispatchSample(callee, stack, PATHS[r.path + 1], r.task)
    end
    return dispatches
end

function show_callee(io::IO, mi::MethodInstance)
    def = mi.def
    if def isa Method
        # with its module, to tell apart e.g. the `__init__` of different packages
        Base.show_tuple_as_call(io, def.name, mi.specTypes; qualified = true)
    else
        Base.print(io, "top-level thunk")
    end
end

# runtime functions that call `jl_apply_generic` for Julia code, and what they do
const VIA = Dict{Symbol, String}(
    :jl_f__apply_iterate => "splat",
    :jl_f_invokelatest => "invokelatest",
    :jl_f__call_latest => "invokelatest",
    :jl_f_invoke_in_world => "invoke_in_world",
    :jl_f__call_in_world => "invoke_in_world",
    :jl_f_finalizer => "finalizer",
    :run_finalizer => "finalizer", # run by the GC, wherever it happened to be
    :do_call => "interpreted",
    :do_apply => "interpreted",
)
strip_i(func::Symbol) = (s = String(func); startswith(s, "ijl_") ? Symbol(chopprefix(s, "i")) : func)

# The Julia frame a dispatch is attributed to: the innermost one, and what in the
# runtime made the call when it was not compiled code calling directly.
function call_site(stack::StackTrace)
    via = ""
    for frame in stack
        if frame.from_c
            isempty(via) && (via = get(VIA, strip_i(frame.func), ""))
        else
            return frame, via
        end
    end
    return nothing, via
end

# The backtraces as a tree, each with the callee as its innermost frame.
function tree(io::IO, data::DispatchResults, fmt::ProfileFormat)
    callee_frames = Dict{MethodInstance, StackFrame}()
    samples = map(data.dispatches) do d
        callee = get!(callee_frames, d.callee) do
            def = d.callee.def
            file, line = def isa Method ? (def.file, Int(def.line)) : (Symbol(""), 0)
            StackFrame(Symbol("→ ", sprint(show_callee, d.callee)), file, line, nothing, false, false, 0)
        end
        Allocs.Alloc(d.callee, pushfirst!(copy(d.stacktrace), callee), 1, d.task, 0)
    end
    root = Allocs.tree!(StackFrameTree{StackFrame}(), samples, fmt.C, fmt.recur)
    print_tree(io, root, Base.displaysize(io)[2], fmt, false)
    Base.println(io, "Total samples: ", length(samples))
    return
end

# `file:line` the way `Profile.print` shows it: the path inside the package after
# `@Package` in the color of the package, linking to the file
function location(file::Symbol, line::Int, filenamemap::FileNameMap)
    path, pkgname, filename = short_path(file, filenamemap)
    sep = Base.Filesystem.path_separator
    linenum = line == -1 ? "?" : string(line)
    loc = if isempty(pkgname)
        styled"$(basename(filename)):$linenum"
    else
        startswith(filename, sep) || (filename = sep * filename)
        pkgcolor = Base.get_stacktrace_color(pkgname, PACKAGE_FIXEDCOLORS)
        styled"{$pkgcolor:$pkgname}$filename:$linenum"
    end
    isempty(path) && return loc
    link = editor_link(path, line)
    return styled"{link=$link:$loc}"
end

# a caller of a call site, without what tells specializations of it apart
const Caller = Tuple{Symbol, Symbol, Int}

# A chain of callers as text, with as many of them as fit in `cols` columns
function chain_text(chain::Vector{Caller}, cols::Int, filenamemap::FileNameMap)
    isempty(chain) && return styled"{light_black:(no Julia caller)}"
    text = styled""
    for (j, (func, file, line)) in enumerate(chain)
        arrow = j > 1 ? " ← " : ""
        loc = location(file, line, filenamemap)
        piece = styled"$arrow$func at $loc"
        if j > 1 && textwidth(text) + textwidth(piece) > cols - 4
            return text * " ← …"
        end
        text *= piece
    end
    return text
end

struct Site
    frame::Union{Nothing, StackFrame}
    via::String
end
Base.:(==)(a::Site, b::Site) = a.via == b.via && (a.frame === nothing) == (b.frame === nothing) &&
    (a.frame === nothing || (a.frame.func, a.frame.file, a.frame.line, a.frame.linfo) == (b.frame.func, b.frame.file, b.frame.line, b.frame.linfo))
Base.hash(s::Site, h::UInt) = s.frame === nothing ? hash(s.via, h) :
    hash((s.frame.func, s.frame.file, s.frame.line, s.via), h)

mutable struct SiteStats
    count::Int
    slow::Int # samples that missed the call-site cache
    callees::Dict{MethodInstance, Int}
    chains::Dict{Vector{Caller}, Int} # the Julia frames above the call site, innermost first
end

# The `n` Julia frames that follow the call site in `stack`
function caller_chain(stack::StackTrace, n::Int)
    chain = Caller[]
    seen_site = false
    for frame in stack
        frame.from_c && continue
        if !seen_site
            seen_site = true
            continue
        end
        length(chain) == n && break
        push!(chain, (frame.func, frame.file, frame.line))
    end
    return chain
end

"""
    Profile.Dispatch.print([io::IO = stdout,] [data::DispatchResults = fetch()]; kwargs...)

Print the call sites that dispatch dynamically the most, each with the methods it
called. The count of a call site is its number of samples; when all samples were
taken at the same rate, the header also gives the estimated number of dispatches.

A call site is the innermost Julia frame of a sample: the source line of the call, with
the file shown as its path inside its package, as in `Profile.print`.
Calls made by the runtime on behalf of that line are marked with what made them, such
as `splat` for `f(args...)`. A site marked `finalizer` is where the garbage collector
happened to run a finalizer; the call is not in the source there. The share of the samples at a call site that missed the
call-site cache is shown when it is not zero: those dispatches are more expensive. A
callee whose compiled signature is not concrete (`@nospecialize`d or `Vararg` arguments,
a type being called) is never in that cache, so its site misses every time; otherwise a
high share means that the site sees many different argument types.

The keyword arguments are:

- `limit = 20`: the number of call sites to show.
- `callees = 3`: the number of callees to show per call site.
- `callers = 0`: when positive, also show how each call site was reached: the chains of
  that many Julia frames above it, innermost first, most frequent chain first (as many
  chains as `callees`). Needs samples recorded with a `maxdepth` that reaches them.
- `mincount = 1`: only show call sites with at least this many samples.
- `format = :sites`: with `:tree`, print the samples as a tree of their backtraces
  instead, like `Profile.print` does, to see how each call site was reached. The count
  of a frame is the number of sampled dispatches below it, and every branch ends in the
  method that was called, marked with `→`. The tree takes the keyword arguments `C`,
  `maxdepth`, `mincount`, `noisefloor`, `sortedby` and `recur` of `Profile.print`. It is
  only as deep as the `maxdepth` the samples were recorded with. Under recursive code
  every depth of the recursion is a branch of its own; `recur = :flat` merges them.
"""
print(; kwargs...) = print(stdout, fetch(); kwargs...)
print(io::IO; kwargs...) = print(io, fetch(); kwargs...)
print(data::DispatchResults; kwargs...) = print(stdout, data; kwargs...)

function print(
        io::IO, data::DispatchResults; format::Symbol = :sites, limit::Int = 20, callees::Int = 3, callers::Int = 0, mincount::Int = 1,
        C::Bool = false, maxdepth::Int = typemax(Int), noisefloor = 0, sortedby::Symbol = :filefuncline, recur::Symbol = :off
    )
    total = length(data.dispatches)
    if total == 0
        @warn """
        There were no samples collected.
        Run your program longer, or record every dispatch with `sample_rate = 1`."""
        return
    end
    if format === :tree
        return tree(io, data, ProfileFormat(; C, maxdepth, mincount, noisefloor, sortedby, recur))
    end
    format === :sites || throw(ArgumentError("output format $(repr(format)) not recognized"))
    sites = Dict{Site, SiteStats}()
    for d in data.dispatches
        stats = get!(() -> SiteStats(0, 0, Dict{MethodInstance, Int}(), Dict{Vector{Caller}, Int}()), sites, Site(call_site(d.stacktrace)...))
        stats.count += 1
        stats.slow += d.path !== :callsite
        stats.callees[d.callee] = get(stats.callees, d.callee, 0) + 1
        if callers > 0
            chain = caller_chain(d.stacktrace, callers)
            stats.chains[chain] = get(stats.chains, chain, 0) + 1
        end
    end
    ranked = sort!(collect(sites); by = p -> p.second.count, rev = true)
    Base.print(io, "Dynamic dispatch: ", total, total == 1 ? " sample" : " samples")
    rate = data.sample_rate
    if rate !== nothing && 0 < rate < 1
        Base.print(io, " (about ", round(Int, total / rate), " dispatches at sample_rate = ", rate, ")")
    end
    Base.println(io, " from ", length(ranked), length(ranked) == 1 ? " call site" : " call sites")
    Base.println(io)
    width = max(ndigits(ranked[1].second.count), 5)
    Base.println(io, lpad("Count", width), "       %  Call site")
    indent = " "^(width + 10)
    cols = Base.displaysize(io)[2]
    filenamemap = FileNameMap()
    # the text of `f(io)`, cut to what is left of a line after `used` columns
    fit(f, used) = Base.rtruncate(sprint(f; context = io), max(cols - used, 20))
    shown = 0
    for (site, stats) in ranked
        (shown >= limit || stats.count < mincount) && break
        shown += 1
        Base.print(io, lpad(stats.count, width), "  ", lpad(round(100 * stats.count / total; digits = 1), 5), "%  ")
        notes = String[]
        # the interpreter's own frames are not decoded, so the frame is the one that ran it
        interpreted = site.via == "interpreted"
        interpreted && Base.print(io, "interpreted code under ")
        interpreted || isempty(site.via) || push!(notes, site.via)
        if stats.slow != 0
            pct = round(Int, 100 * stats.slow / stats.count)
            push!(notes, string(pct == 0 ? "<1" : pct, "% missed the call-site cache"))
        end
        note = isempty(notes) ? "" : string(" [", join(notes, ", "), "]")
        if site.frame === nothing
            printstyled(io, "(no Julia frame)"; color = :light_black)
        else
            # cut the signature, not the location after it
            frame = site.frame
            loc = location(frame.file, frame.line, filenamemap)
            inlined = frame.inlined ? " [inlined]" : ""
            used = width + 14 + textwidth(loc) + textwidth(inlined) + textwidth(note) + (interpreted ? 23 : 0)
            Base.print(io, fit(io -> Base.StackTraces.show_spec_linfo(io, frame), used), " at ", loc, inlined)
        end
        printstyled(io, note, "\n"; color = :light_black)
        targets = sort!(collect(stats.callees); by = last, rev = true)
        for (i, (mi, count)) in enumerate(targets)
            if i > callees
                printstyled(io, indent, "… and ", length(targets) - callees, " more\n"; color = :light_black)
                break
            end
            printstyled(io, indent, lpad(count, width), "  "; color = :light_black)
            Base.println(io, fit(io -> show_callee(io, mi), 2 * width + 12))
        end
        callers > 0 || continue
        # the chains as they are shown: those that only differ in what does not fit are one
        shown_chains = Dict{String, Tuple{Any, Int}}()
        for (chain, count) in stats.chains
            text = chain_text(chain, max(cols - 2 * width - 12, 20), filenamemap)
            _, n = get(shown_chains, String(text), (text, 0))
            shown_chains[String(text)] = (text, n + count)
        end
        chains = sort!(collect(values(shown_chains)); by = last, rev = true)
        printstyled(io, indent, "called from\n"; color = :light_black)
        for (i, (text, count)) in enumerate(chains)
            if i > callees
                printstyled(io, indent, "… and ", length(chains) - callees, " more\n"; color = :light_black)
                break
            end
            printstyled(io, indent, lpad(count, width), "  "; color = :light_black)
            Base.println(io, text)
        end
    end
    if shown < length(ranked)
        printstyled(io, "… and ", length(ranked) - shown, " more call sites\n"; color = :light_black)
    end
    return
end

end
