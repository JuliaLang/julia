# Diagnostic for JuliaLang/julia#55900 (not for merging).
#
# On i686 Windows, backtraces intermittently lose two adjacent frames. All Julia
# code is compiled with "frame-pointer"="all" and libjulia with
# -fno-omit-frame-pointer, so the saved frame-pointer chain is an independent
# record of the stack. `check` captures a backtrace with stack pointers while
# the frames are still live, walks the chain itself, and reports every frame on
# which the two disagree. Everything it prints starts with "W32BT".
module W32BT

using Base.StackTraces: lookup

const W = sizeof(UInt)
const NONPTR = typemax(UInt)  # JL_BT_NON_PTR_ENTRY
const ON_X86 = Sys.ARCH in (:i686, :x86_64)

# Read one word, without faulting on a bad address.
@static if Sys.iswindows()
    function rd(a::UInt)
        r = Ref{UInt}(0)
        ok = ccall(:ReadProcessMemory, stdcall, Cint,
                   (Ptr{Cvoid}, UInt, Ptr{UInt}, Csize_t, Ptr{Csize_t}),
                   Ptr{Cvoid}(NONPTR), a, r, W, C_NULL)  # (HANDLE)-1 is the current process
        return ok == 0 ? nothing : r[]
    end
else
    # Only called on addresses inside the range of reported stack pointers.
    rd(a::UInt) = unsafe_load(Ptr{UInt}(a))
end

hex(x) = x === nothing ? "unreadable" : string(x, base=16)

function name(ip::UInt)
    fr = lookup(Ptr{Cvoid}(ip))
    isempty(fr) && return "?"
    f = fr[end]
    return string(f.func, f.from_c ? " (C)" : "")
end

# Native frames reported by the runtime's unwinder: (ip, sp), innermost first.
# Stops at the first extended (interpreter) entry, whose sp slot isn't meaningful.
function capture()
    r = ccall(:jl_backtrace_from_here, Ref{Core.SimpleVector}, (Cint, Cint), 1, 0)
    ips = r[1]::Vector{Ptr{Cvoid}}; sps = r[3]::Vector{Ptr{Cvoid}}
    n = 0
    while n < length(ips) && UInt(ips[n+1]) != NONPTR
        n += 1
    end
    return UInt.(ips[1:n]), UInt.(sps[1:n]), length(ips)
end

# The recursion frames in `bt`, by function name, innermost first.
function rec_names(bt, names)
    out = Symbol[]
    for fr in Base.stacktrace(bt)
        fr.func in names && push!(out, fr.func)
    end
    return out
end

# Expected names of the errorshow recursions, innermost first.
function expected_nested()
    s = Symbol[]
    f(n) = (push!(s, :f28442a); n ≤ 0 ? nothing : g(n - 1))
    g(n) = (push!(s, :g28442a); 80 > n > 20 ? h(n - 1) : f(n - 1))
    h(n) = (push!(s, :h28442a); n % 10 == 0 ? g(n - 1) : h(n - 1))
    f(100)
    return reverse!(s)
end
function expected_long()
    s = Symbol[]
    f(c) = (push!(s, :f28442); g(c + 1))
    g(c) = (push!(s, :g28442); c > 10000 ? nothing : f(c + 1))
    f(1)
    return reverse!(s)
end
const NAMES = Dict("nested" => [:f28442a, :g28442a, :h28442a], "long" => [:f28442, :g28442])
const EXPECTED = Dict{String,Vector{Symbol}}()
expected(kind) = get!(() -> kind == "nested" ? expected_nested() : expected_long(), EXPECTED, kind)

# Index of the first mismatch between the recursion frames in `bt` and the
# expected sequence, or 0 if they agree.
function layout_mismatch(kind, bt)
    got = rec_names(bt, NAMES[kind]); exp = expected(kind)
    for i in 1:min(length(got), length(exp))
        got[i] == exp[i] || return i
    end
    return length(got) == length(exp) ? 0 : min(length(got), length(exp)) + 1
end

# Walk the saved frame-pointer chain upward from frame `k0`, whose frame pointer
# is `fp0`. Returns (caller_sp, return_address) for each link: caller_sp is
# what the unwinder should report as the next frame's sp (fp + 2W).
function walk_chain(fp0, hi)
    links = Tuple{UInt,UInt}[]
    fp = fp0
    while length(links) < 100_000
        ra = rd(fp + W)
        ra === nothing && break
        push!(links, (fp + 2W, ra))
        nfp = rd(fp)
        (nfp === nothing || nfp <= fp || nfp > hi) && break
        fp = nfp
    end
    return links
end

# Compare the reported frames with the chain. Returns a list of report lines;
# empty means they agree everywhere both cover.
function chain_diff(tag, ip, sp)
    n = length(ip)
    # Anchor: the first frame whose step to its caller is a plain frame-pointer step.
    k0 = 0
    for k in 1:n-1
        fp = sp[k+1] - 2W
        if sp[k] <= fp && rd(fp + W) == ip[k+1] + 1
            k0 = k
            break
        end
    end
    k0 == 0 && return (["W32BT [$tag] no frame-pointer anchor in $n frames; sp[1:4]=$(hex.(sp[1:min(4,n)]))"], 0, 0)
    hi = Sys.iswindows() ? typemax(UInt) : sp[n]
    links = walk_chain(sp[k0+1] - 2W, hi)
    # Merge the two increasing sp sequences: reported frames k0+1..n and chain links.
    lines = String[]
    i = k0 + 1; j = 1; matched = 0; skipped = 0; extra = 0; badra = 0
    first_sp = UInt(0)
    while i <= n && j <= length(links)
        csp, cra = links[j]
        if sp[i] == csp
            if cra != ip[i] + 1
                badra += 1
                first_sp == 0 && (first_sp = sp[i])
                push!(lines, "W32BT [$tag]   frame $i sp=$(hex(sp[i])): walker ip=$(hex(ip[i])) $(name(ip[i])), chain ra-1=$(hex(cra-1)) $(name(cra-1))")
            end
            matched += 1; i += 1; j += 1
        elseif csp < sp[i]
            skipped += 1
            first_sp == 0 && (first_sp = csp)
            push!(lines, "W32BT [$tag]   SKIPPED before frame $i: chain sp=$(hex(csp)) ra-1=$(hex(cra-1)) $(name(cra-1))")
            j += 1
        else
            extra += 1
            first_sp == 0 && (first_sp = sp[i])
            push!(lines, "W32BT [$tag]   EXTRA frame $i: walker sp=$(hex(sp[i])) ip=$(hex(ip[i])) $(name(ip[i])) (not on the chain)")
            i += 1
        end
    end
    isempty(lines) && return (lines, matched, length(links))
    # Context: the reported frames and raw stack words around the first problem.
    pushfirst!(lines, "W32BT [$tag] chain disagrees with the unwinder: anchor=$k0 reported=$n chain=$(length(links)) matched=$matched skipped=$skipped extra=$extra bad_ra=$badra")
    lo = findlast(<(first_sp), sp)
    lo === nothing && (lo = 1)
    for k in max(1, lo - 3):min(n, lo + 4)
        push!(lines, "W32BT [$tag]     reported $k: sp=$(hex(sp[k])) ip=$(hex(ip[k])) $(name(ip[k]))")
    end
    if 2 <= lo + 1 <= n
        a = sp[max(1, lo - 1)]; b = sp[min(n, lo + 2)]
        ws = String[]
        for x in a:W:min(b, a + 160W)
            push!(ws, hex(rd(x)))
        end
        push!(lines, "W32BT [$tag]     words $(hex(a))..$(hex(b)): " * join(ws, " "))
    end
    length(lines) > 80 && (lines = [lines[1:80]; "W32BT [$tag]   ... $(length(lines) - 80) more lines"])
    return (lines, matched, length(links))
end

# Full dump of a backtrace with C frames, around the first layout mismatch.
function dump_bt(tag, kind, bt, mm)
    lines = String[]
    st = Base.stacktrace(bt, true)
    names = NAMES[kind]
    # Find the C-inclusive index of the mm-th recursion frame.
    c = 0; at = length(st)
    for (k, fr) in enumerate(st)
        fr.func in names && (c += 1)
        c == mm && (at = k; break)
    end
    for k in max(1, at - 25):min(length(st), at + 10)
        fr = st[k]
        push!(lines, "W32BT [$tag]     bt $k: ip=$(hex(UInt(fr.pointer))) $(fr.func)$(fr.from_c ? " (C)" : "")")
    end
    return lines
end

"""
    check(tag, kind, bt)

Call at the bottom of an errorshow-style recursion, right after `bt = backtrace()`,
while the recursion frames are live. `kind` is "nested" or "long". Prints only
when something is wrong; returns a NamedTuple summary.
"""
function check(tag, kind, bt; drop = 0)
    ON_X86 || return (; layout_bt = 0, layout_own = 0, gaps = 0, nframes = 0, matched = 0, nchain = 0)
    ip, sp, ntotal = capture()
    if drop > 0  # positive control: pretend the unwinder skipped native frames drop and drop+1
        deleteat!(ip, [drop, drop + 1]); deleteat!(sp, [drop, drop + 1])
    end
    own = ccall(:jl_backtrace_from_here, Ref{Core.SimpleVector}, (Cint, Cint), 0, 0)
    own_bt = Base._reformat_bt(own[1], own[2])
    mm_bt = layout_mismatch(kind, bt)
    mm_own = layout_mismatch(kind, own_bt)
    diff, matched, nchain = chain_diff(tag, ip, sp)
    lines = copy(diff)
    if mm_bt != 0 || mm_own != 0
        pushfirst!(lines, "W32BT [$tag] LAYOUT WRONG: first mismatch at recursion frame $mm_bt in backtrace(), $mm_own in a second capture" *
                   (isempty(diff) ? "; the chain check found NOTHING, so it is blind to this case" : ""))
        mm_bt != 0 && append!(lines, dump_bt(tag * " backtrace()", kind, bt, mm_bt))
        if isempty(diff)
            # The diagnostic missed it: dump every native frame and the chain, for offline comparison.
            n = length(ip)
            for k in 1:min(n, 600)
                push!(lines, "W32BT [$tag]     all $k: sp=$(hex(sp[k])) ip=$(hex(ip[k])) $(name(ip[k]))")
            end
        end
    end
    isempty(lines) || println(join(lines, "\n"))
    return (; layout_bt = mm_bt, layout_own = mm_own, gaps = length(diff), nframes = length(ip), matched, nchain)
end

# Called after an errorshow testset, with the backtrace it captured (stack gone).
function report_layout(tag, kind, bt)
    mm = layout_mismatch(kind, bt)
    mm == 0 && return 0
    println(join(["W32BT [$tag] LAYOUT WRONG: first mismatch at recursion frame $mm";
                  dump_bt(tag, kind, bt, mm)], "\n"))
    return mm
end

end # module W32BT
