# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random

# Tiered execution is switched on only around the calls under test, with low thresholds,
# so that freshly defined functions start out interpreted and their loops get hot quickly.
# Each case is checked against a separately defined copy of the same function that is
# compiled the usual way.

const Tiered = Base.Tiered

tier_threshold(which) = ccall(:jl_tier_get_threshold, UInt32, (Cint,), which)
set_tier_threshold(which, v) = ccall(:jl_tier_set_threshold, Cvoid, (Cint, UInt32), which, v)
tier_enabled() = ccall(:jl_tier_is_enabled, Cint, ()) != 0
set_tier_enabled(on) = ccall(:jl_tier_set_enabled, Cvoid, (Cint,), on)

small_cost() = ccall(:jl_tier_get_small_cost, UInt32, ())
set_small_cost(v) = ccall(:jl_tier_set_small_cost, Cvoid, (UInt32,), v)

# Budgets of `work` and `loop` statements, without the cap for small methods unless `small`
function with_tiering(f; work=20, loop=50, small=0)
    old = (tier_enabled(), tier_threshold(0), tier_threshold(1), small_cost())
    set_tier_threshold(0, work)
    set_tier_threshold(1, loop)
    set_small_cost(small)
    set_tier_enabled(true)
    try
        return f()
    finally
        set_tier_enabled(old[1])
        set_tier_threshold(0, old[2])
        set_tier_threshold(1, old[3])
        set_small_cost(old[4])
    end
end

# Define a fresh copy of `def`, a `function _(args...) ... end` expression, and return it.
function fresh(def::Expr)
    def = deepcopy(def)
    sig = def.args[1]
    while Meta.isexpr(sig, :where)
        sig = sig.args[1]
    end
    @assert Meta.isexpr(sig, :call) && sig.args[1] === :_
    name = gensym(:tiered)
    sig.args[1] = name
    Core.eval(@__MODULE__, def)
    return Core.eval(@__MODULE__, name)
end

function outcome(f, args...)
    try
        return (:ok, Base.invokelatest(f, args...))
    catch err
        return (:error, typeof(err), sprint(showerror, err))
    end
end

# Statements interpreted in the specialization of `f` for argument type `T`, or in all of
# its specializations.
mi_work(mi::Core.MethodInstance) = Int(ccall(:jl_tier_mi_work, UInt32, (Any,), mi))
function mi_work(f, T)
    mi = Base.method_instance(f, (T,))
    return mi === nothing ? 0 : mi_work(mi)
end
total_work(f) = sum(mi_work, Base.specializations(only(methods(f))); init=0)

function iscompiled(f, T)
    mi = Base.method_instance(f, (T,))
    return mi !== nothing && isdefined(mi, :cache) && mi.cache.invoke != C_NULL
end

"""
    check(def, args...; entered=true)

Run a fresh copy of `def` with tiering on and compare its outcome (value or error) with a
copy compiled as usual. With `entered`, also require that a loop continuation ran.
"""
function check(def::Expr, args...; entered::Bool=true, kws...)
    expected = outcome(fresh(def), args...)
    f = fresh(def)
    before = Tiered.stats[2]
    got = with_tiering(; kws...) do
        outcome(f, args...)
    end
    @test isequal(got, expected)
    if entered
        @test Tiered.stats[2] > before
    end
    return got
end

@testset "tier gate" begin
    f = fresh(:(function _(x) x + 1 end))
    with_tiering(work=1000) do
        for i in 1:3
            @test Base.invokelatest(f, i) == i + 1
        end
    end
    # the budget counts statements interpreted, at least one per call
    w = mi_work(f, Int)
    @test 3 <= w <= 30
    @test !iscompiled(f, Int)
    # per specialization
    with_tiering(work=1000) do
        @test Base.invokelatest(f, 1.0) == 2.0
    end
    @test mi_work(f, Int) == w
    @test mi_work(f, Float64) > 0
    # once its budget is spent a specialization is compiled, and only that one
    with_tiering(work=w) do
        @test Base.invokelatest(f, 4) == 5
        @test Base.invokelatest(f, 2.0) == 3.0
    end
    @test iscompiled(f, Int)
    @test !iscompiled(f, Float64)

    # code that needs the compiler is never interpreted
    g = fresh(:(function _(x) ccall(:abs, Cint, (Cint,), x) end))
    with_tiering() do
        @test Base.invokelatest(g, -3) == 3
    end
    @test total_work(g) == 0
    h = fresh(:(function _(x) Base.Experimental.@force_compile; x end))
    with_tiering() do
        @test Base.invokelatest(h, 3) == 3
    end
    @test total_work(h) == 0
    # nor is code using `@fastmath`, whose operations compiled code may fuse
    def = :(function _(x, y) @fastmath x * y - 1.0 end)
    fm, ref = fresh(def), fresh(def)
    with_tiering() do
        @test Base.invokelatest(fm, 0.1, 10.0) === ref(0.1, 10.0)
    end
    @test total_work(fm) == 0
    # or `muladd`, called or passed on, whose fusing can depend on inlining
    ma = fresh(:(function _(x, y) muladd(x, y, -1.0) end))
    mb = fresh(:(function _(x) broadcast(muladd, x, 10.0, -1.0) end))
    with_tiering() do
        @test Base.invokelatest(ma, 0.1, 10.0) === muladd(0.1, 10.0, -1.0)
        @test Base.invokelatest(mb, [0.1]) == [muladd(0.1, 10.0, -1.0)]
    end
    @test total_work(ma) == 0
    @test total_work(mb) == 0
end

@testset "single long loop, then output" begin
    r = check(:(function _(n)
        io = IOBuffer()
        s = 0
        for i in 1:n
            s += i
        end
        println(io, "sum: ", s)
        String(take!(io))
    end), 100_000)
    @test r == (:ok, "sum: 5000050000\n")
end

@testset "while loop with an early return" begin
    check(:(function _(n)
        i = 0
        acc = 1.0
        while true
            i += 1
            acc = acc * 1.0001
            acc > n && return (i, acc)
        end
    end), 2.0)
end

@testset "many calls with short loops" begin
    inner = fresh(:(function _(n)
        s = 0
        for i in 1:n
            s += i * i
        end
        s
    end))
    outer = fresh(:(function _(f, k)
        t = 0
        for j in 1:k
            t += f(j % 7)
        end
        t
    end))
    expected = sum(sum(i * i for i in 1:(j % 7); init=0) for j in 1:2000)
    @test with_tiering(work=50) do
        Base.invokelatest(outer, inner, 2000)
    end == expected
end

@testset "nested loops" begin
    check(:(function _(n)
        s = 0.0
        for i in 1:n
            for j in 1:i
                s += i / j
            end
        end
        s
    end), 300)
    # entered at the inner header, the continuation runs the whole outer loop
    check(:(function _(n)
        s = 0
        k = 0
        while k < n
            k += 1
            j = 0
            while j < 1000
                j += 1
                s += j % k
            end
        end
        (s, k)
    end), 50)
end

@testset "loops in sequence" begin
    check(:(function _(n)
        a = 0
        for i in 1:n
            a += i
        end
        b = 1.0
        for i in 1:n
            b += sqrt(i)
        end
        c = ""
        for i in 1:div(n, 100)
            c *= string(i % 10)
        end
        (a, b, c)
    end), 10_000)
end

@testset "exits" begin
    # break to different places
    def = :(function _(n, m)
        s = 0
        for i in 1:n
            if i == m
                s = -s
                break
            end
            s += i
            if s > 10^9
                s = 0
                break
            end
        end
        s
    end)
    check(def, 1000, 500)
    check(def, 10^6, 10^7)
    check(def, 1000, 10^7)
    # return from inside the loop
    def = :(function _(v, x)
        for i in eachindex(v)
            v[i] == x && return i
        end
        return nothing
    end)
    check(def, collect(1:10_000), 9_000)
    check(def, collect(1:10_000), -1)
    # @goto out of the loop
    check(:(function _(n)
        s = 0
        for i in 1:n
            s += i
            s > 50_000 && @goto done
        end
        return (:normal, s)
        @label done
        return (:jumped, s)
    end), 10_000)
    # a variable the loop only sometimes writes, and never reads
    def = :(function _(v)
        found = false
        for x in v
            if x == 0
                found = true
            end
        end
        found
    end)
    check(def, collect(1:10_000))
    check(def, [1:5000; 0; 1:5000])
    # a loop body that throws
    check(:(function _(n)
        s = 0
        for i in 1:n
            s += i
            i == 5000 && error("stop at ", s)
        end
        s
    end), 10_000)
end

@testset "exceptions" begin
    # try inside the loop
    check(:(function _(n)
        s = 0
        caught = 0
        for i in 1:n
            try
                i % 1000 == 0 && error("boom")
                s += i
            catch
                caught += 1
            end
        end
        (s, caught)
    end), 10_000)
    # try inside the loop, with an exit from inside the try
    check(:(function _(n)
        s = 0
        for i in 1:n
            try
                s += i
                s > 1_000_000 && break
            finally
                s += 1
            end
        end
        s
    end), 100_000)
    # loop inside a try: the catch block sees the values from the loop
    check(:(function _(n)
        s = 0
        i = 0
        try
            while true
                i += 1
                s += i
                i == n && error("done")
            end
        catch err
            return (i, s, sprint(showerror, err))
        end
    end), 20_000)
    # loop inside a catch block
    check(:(function _(n)
        try
            error("x")
        catch err
            s = 0
            for i in 1:n
                s += length(sprint(showerror, err))
            end
            return s
        end
    end), 2000)
    # finally around a loop that returns
    check(:(function _(v)
        log = Int[]
        try
            for x in v
                x < 0 && return (x, log)
                push!(log, x)
            end
        finally
            push!(log, 0)
        end
        (nothing, log)
    end), [1:5000; -1; 1:10])
end

const tier_sv = Base.ScopedValues.ScopedValue(0)

@testset "scoped values" begin
    # a scope entered and left (also by `break`) inside the loop
    check(:(function _(n)
        s = 0
        for i in 1:n
            Base.ScopedValues.@with tier_sv => i begin
                s += tier_sv[]
                s > 10^7 && break
            end
        end
        s
    end), 10_000)
    # a loop inside a scope
    check(:(function _(n)
        Base.ScopedValues.@with tier_sv => 3 begin
            s = 0
            for i in 1:n
                s += tier_sv[]
            end
            s
        end
    end), 10_000)
end

@testset "type changes" begin
    # loop-carried value changes type in the first iteration
    check(:(function _(n)
        x = 0
        for i in 1:n
            x += 0.5
        end
        x
    end), 10_000)
    # and repeatedly
    check(:(function _(n)
        x = 0
        for i in 1:n
            x = i % 3 == 0 ? x + 1 : (i % 3 == 1 ? Float64(x) : Int32(x))
        end
        (x, typeof(x))
    end), 10_000)
    # a variable that is only sometimes defined
    check(:(function _(n)
        local y
        s = 0
        for i in 1:n
            if i % 100 == 0
                y = i
            end
            s += @isdefined(y) ? y : 0
        end
        s
    end), 10_000)
end

@testset "varargs, closures, static parameters" begin
    check(:(function _(xs...)
        s = 0
        for k in 1:5000
            for x in xs
                s += x * k
            end
        end
        s
    end), 1, 2, 3)
    check(:(function _(n)
        c = 0
        add!(k) = (c += k)  # c is captured in a Core.Box
        for i in 1:n
            add!(i)
        end
        c
    end), 10_000)
    check(:(function _(v::Vector{T}, n) where {T}
        s = zero(T)
        for i in 1:n
            s += T(i) * v[1 + i % length(v)]
        end
        (s, T)
    end), Float32[1, 2, 3], 10_000)
    check(:(function _(::Val{N}, n) where {N}
        s = 0
        for i in 1:n
            s += N
        end
        s
    end), Val(3), 10_000)
end

@testset "dispatch over many specializations" begin
    leaf = fresh(:(function _(::Val{N}) where {N} N end))
    expected = sum(i % 200 for i in 1:5000)
    got = with_tiering(work=1000) do
        s = 0
        for i in 1:5000
            s += Base.invokelatest(leaf, Val(i % 200))::Int # always dynamic dispatch
        end
        s
    end
    @test got == expected
    # each specialization is cheap and called rarely, so none is compiled on its own,
    # however often the method as a whole is called
    @test !iscompiled(leaf, Val{199})
    @test 0 < mi_work(leaf, Val{199}) < 1000
    # and a driver whose loop is compiled still computes the same thing
    driver = fresh(:(function _(k)
        s = 0
        for i in 1:k
            s += $leaf(Val(i % 200))
        end
        s
    end))
    @test with_tiering(work=50) do
        Base.invokelatest(driver, 5000)
    end == expected
end

@testset "recursion" begin
    check(:(function _(n)
        fib(k) = k < 2 ? k : fib(k - 1) + fib(k - 2)
        fib(n)
    end), 20; entered=false)
    # recursion with loops in each frame
    check(:(function _(n)
        f(k) = k == 0 ? 0 : (s = 0; for i in 1:1000; s += i % k; end; s + f(k - 1))
        f(n)
    end), 30)
    # a hot recursion moves to compiled code at its next call, partway through
    mod = Module()
    fib = Core.eval(mod, :(fib(k) = k < 2 ? k : fib(k - 1) + fib(k - 2)))
    @test with_tiering(work=1000) do
        Base.invokelatest(fib, 20)
    end == 6765
    @test iscompiled(fib, Int)
end

@testset "redefinition" begin
    mod = Module()
    Core.eval(mod, :(g(x) = x + 1))
    Core.eval(mod, :(function run(n)
        s = 0
        for i in 1:n
            s += g(i)
        end
        s
    end))
    with_tiering() do
        @test Base.invokelatest(mod.run, 1000) == sum(i + 1 for i in 1:1000)
        # a cached continuation must not run stale code
        Core.eval(mod, :(g(x) = 2x))
        @test Base.invokelatest(mod.run, 1000) == sum(2i for i in 1:1000)
    end
    # redefinition while a frame is in the middle of its loop
    Core.eval(mod, :(h(x) = 1))
    Core.eval(mod, :(function run2(n)
        s = 0
        for i in 1:n
            s += h(i)
            i == n ÷ 2 && Core.eval(@__MODULE__, :(h(x) = 2))
        end
        s
    end))
    with_tiering() do
        # the running frame keeps its world: it sees the old definition throughout
        @test Base.invokelatest(mod.run2, 1000) == 1000
        @test Base.invokelatest(mod.run2, 1000) == 2000
    end
end

@testset "@force_compile" begin
    f = fresh(:(function _(n)
        Base.Experimental.@force_compile
        s = 0
        for i in 1:n
            s += i
        end
        s
    end))
    before = Tiered.stats[2]
    with_tiering() do
        @test Base.invokelatest(f, 10_000) == 50005000
    end
    @test Tiered.stats[2] == before
    @test total_work(f) == 0

    # on its own it compiles only its own body: what that calls by dynamic dispatch is tiered
    callee = fresh(:(function _(x) x + 1 end))
    caller = fresh(:(function _(x)
        Base.Experimental.@force_compile
        $callee(Base.inferencebarrier(x))
    end))
    with_tiering(work=1000) do
        @test Base.invokelatest(caller, 1) == 2
    end
    @test total_work(caller) == 0
    @test total_work(callee) > 0

    # given an expression, it turns tiering off for everything that expression runs
    callee = fresh(:(function _(x) x + 1 end))
    caller = fresh(:(function _(x) $callee(Base.inferencebarrier(x)) end))
    spawned = fresh(:(function _(x) x + 1 end))
    with_tiering(work=1000) do
        @test Base.Experimental.@force_compile(Base.invokelatest(caller, 1)) == 2
        @test Base.Experimental.@force_compile(fetch(Threads.@spawn Base.invokelatest(spawned, 1))) == 2
    end
    @test total_work(caller) == 0
    @test total_work(callee) == 0
    @test iscompiled(callee, Int)
    @test total_work(spawned) == 0
    # and turns it back on afterwards, even when the expression throws
    thrower = fresh(:(function _(x) error("boom") end))
    after = fresh(:(function _(x) x + 1 end))
    with_tiering(work=1000) do
        @test_throws ErrorException Base.Experimental.@force_compile Base.invokelatest(thrower, 1)
        @test Base.invokelatest(after, 1) == 2
    end
    @test total_work(after) > 0
end

@testset "measurements run compiled" begin
    f = fresh(:(function _(v)
        s = 0.0
        for x in v
            s += x
        end
        s
    end))
    v = rand(MersenneTwister(1), 1000)
    allocs = with_tiering() do
        Base.invokelatest() do
            f(v)
            @allocated f(v)
        end
    end
    @test allocs == 0
    # tasks started inside a measurement inherit it
    g = fresh(:(function _(x) x + 1 end))
    with_tiering(work=1000) do
        @allocated fetch(Threads.@spawn Base.invokelatest(g, 1))
    end
    @test total_work(g) == 0
end

@testset "budget cap for small methods" begin
    f = fresh(:(_(x) = x + one(x)))
    # a cap of a few statements per statement of the method compiles it within a few calls,
    # though the plain budget is far from spent ...
    with_tiering(work=10^6, loop=10^6, small=2) do
        for _ in 1:20
            @test Base.invokelatest(f, 2) == 3
        end
    end
    @test iscompiled(f, Int)
    # ... and a cap above the plain budget never keeps it interpreted longer
    with_tiering(work=20, loop=10^6, small=10_000) do
        for _ in 1:20
            @test Base.invokelatest(f, 2.0) == 3.0
        end
    end
    @test iscompiled(f, Float64)
end
