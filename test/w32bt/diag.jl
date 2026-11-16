# Diagnostic for JuliaLang/julia#55900: compare the runtime's backtrace with an
# independent walk of the saved frame-pointer chain, while the frames are live.
# All Julia code is compiled with "frame-pointer"="all", so for a frame k whose
# caller is frame k+1 the unwinder should report sp[k+1] == fp_k + 2W and the
# return address at fp_k + W.  Where the reported frames skip over links in the
# chain, print what the chain says was there.
using Base.StackTraces: lookup

const W = sizeof(UInt)
ld(a::UInt) = unsafe_load(Ptr{UInt}(a))

function name(ip)
    fr = lookup(Ptr{Cvoid}(ip))
    isempty(fr) ? "?" : string(fr[end].func, fr[end].from_c ? " (C)" : "")
end

# Returns (nframes, list of gap descriptions)
function check_live(tag; drop=0)
    r = ccall(:jl_backtrace_from_here, Ref{Core.SimpleVector}, (Cint, Cint), 1, 0)
    ips = r[1]::Vector{Ptr{Cvoid}}; sps = r[3]::Vector{Ptr{Cvoid}}
    n = 0  # native prefix only: extended (interpreter) entries break the sp/ip pairing
    while n < length(ips) && UInt(ips[n+1]) != typemax(UInt); n += 1; end
    ip = UInt.(ips[1:n]); sp = UInt.(sps[1:n])
    if drop > 0  # positive control: pretend the unwinder skipped frames drop, drop+1
        deleteat!(ip, [drop, drop+1]); deleteat!(sp, [drop, drop+1]); n -= 2
    end
    gaps = String[]
    nchecked = 0
    for k in 1:n-1
        fp = sp[k+1] - 2W                 # fp of frame k, if the unwinder used the chain
        ld(fp + W) == ip[k+1] + 1 || continue   # this link wasn't a frame-pointer step; skip
        k + 2 <= n || continue
        nchecked += 1
        nfp = ld(fp)                       # saved fp = fp of frame k+1
        nfp + 2W == sp[k+2] && continue    # next step consistent with the chain
        # The walker went somewhere else.  Follow the chain to see what it skipped.
        io = IOBuffer()
        println(io, "[$tag] gap after frame $(k+1) ($(name(ip[k+1]))): sp[$(k+1)]=0x$(string(sp[k+1],base=16)) ",
                "sp[$(k+2)]=0x$(string(sp[k+2],base=16)) ($(name(ip[k+2]))), chain fp=0x$(string(nfp,base=16))")
        f = nfp
        for j in 1:8
            (f == 0 || f + 2W > sp[k+2] + 64W) && break
            ra = ld(f + W)
            println(io, "    chain $j: fp=0x$(string(f,base=16)) ra=0x$(string(ra,base=16)) $(name(ra - 1))",
                    f + 2W == sp[k+2] ? "   <- walker resumes here" : "")
            f + 2W == sp[k+2] && break
            f = ld(f)
        end
        # raw words between the two reported sps, to see what a stack scan could have found
        print(io, "    words sp[$(k+1)]..sp[$(k+2)]:")
        for a in sp[k+1]:W:min(sp[k+2], sp[k+1] + 96W)
            print(io, " ", string(ld(a), base=16))
        end
        println(io)
        push!(gaps, String(take!(io)))
    end
    return (n, gaps, nchecked)
end

f28442a(n) = n ≤ 0 ? (return check_live("nested")) : g28442a(n - 1)
g28442a(n) = 80 > n > 20 ? h28442a(n - 1) : f28442a(n - 1)
h28442a(n) = n % 10 == 0 ? g28442a(n - 1) : h28442a(n - 1)
f28442(c) = g28442(c + 1)
g28442(c) = c > 10000 ? (return check_live("long")) : f28442(c+1)

function run(reps)
    bad = 0
    for i in 1:reps
        for (n, gaps, _) in (f28442a(100), f28442(1))
            isempty(gaps) && continue
            bad += 1
            i <= 3 && foreach(print, gaps)
        end
    end
    return bad
end
backtrace()
nbad = run(parse(Int, get(ARGS, 1, "20")))
let (n, _, c) = f28442a(100); println("W32BT pid=$(getpid()) bad=$nbad nframes=$n links_checked=$c"); end
if get(ENV, "W32BT_CONTROL", "") == "1"
    f28442a(n) = n ≤ 0 ? (return check_live("control"; drop=45)) : g28442a(n - 1)
    (_, gaps, _) = f28442a(100); foreach(print, gaps)
end
