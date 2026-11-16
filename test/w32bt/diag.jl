# Fresh-process part of the JuliaLang/julia#55900 diagnostic (not for merging).
# Runs the errorshow recursions, in both the direct-call form and the boxed-closure
# form errorshow itself uses, and checks each backtrace against the frame chain.
include(joinpath(@__DIR__, "diag_lib.jl"))
using .W32BT

# Direct calls between top-level functions: only Julia frames on the stack.
f28442a(n) = n ≤ 0 ? (bt = backtrace(); return W32BT.check("fresh direct nested", "nested", bt)) : g28442a(n - 1)
g28442a(n) = 80 > n > 20 ? h28442a(n - 1) : f28442a(n - 1)
h28442a(n) = n % 10 == 0 ? g28442a(n - 1) : h28442a(n - 1)
f28442(c) = g28442(c + 1)
g28442(c) = c > 10000 ? (bt = backtrace(); return W32BT.check("fresh direct long", "long", bt)) : f28442(c+1)

# Local closures that capture each other through `Core.Box`, as in errorshow.jl:
# every call is a dynamic dispatch, so libjulia frames sit between Julia frames.
function boxed()
    f28442a(n) = n ≤ 0 ? (bt = backtrace(); return W32BT.check("fresh boxed nested", "nested", bt)) : g28442a(n - 1)
    g28442a(n) = 80 > n > 20 ? h28442a(n - 1) : f28442a(n - 1)
    h28442a(n) = n % 10 == 0 ? g28442a(n - 1) : h28442a(n - 1)
    f28442(c) = g28442(c + 1)
    g28442(c) = c > 10000 ? (bt = backtrace(); return W32BT.check("fresh boxed long", "long", bt)) : f28442(c+1)
    return (f28442a(100), f28442(1))
end

backtrace()
reps = parse(Int, get(ARGS, 1, "5"))
bad = 0; nf = (0, 0)
for i in 1:reps
    global bad, nf
    rs = (f28442a(100), f28442(1), boxed()...)
    bad += count(r -> r.gaps > 0 || r.layout_bt != 0 || r.layout_own != 0, rs)
    nf = ("$(rs[1].nframes)/$(rs[1].matched)", "$(rs[3].nframes)/$(rs[3].matched)")
end
println("W32BT pid=$(getpid()) bad=$bad direct_nframes/matched=$(nf[1]) boxed_nframes/matched=$(nf[2])")
