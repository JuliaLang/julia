# This file is a part of Julia. License is MIT: https://julialang.org/license

isdefined(Main, :FakePTYs) || @eval Main include("testhelpers/FakePTYs.jl")
include("testhelpers/withlocales.jl")

# Tests that do not really go anywhere else

# test @assert macro
@test_throws AssertionError (@assert 1 == 2)
@test_throws AssertionError (@assert false)
@test_throws AssertionError (@assert false "this is a test")
@test_throws AssertionError (@assert false "this is a test" "another test")
@test_throws AssertionError (@assert false :a)
let
    try
        @assert 1 == 2
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test occursin("1 == 2", ex.msg)
    end
end
# test @assert message
let
    try
        @assert 1 == 2 "this is a test"
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "this is a test"
    end
end
# @assert only uses the first message string
let
    try
        @assert 1 == 2 "this is a test" "this is another test"
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "this is a test"
    end
end
# @assert calls string() on second argument
let
    try
        @assert 1 == 2 :random_object
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test !occursin("1 == 2", ex.msg)
        @test occursin("random_object", ex.msg)
    end
end
# if the second argument is an expression, c
let deepthought(x, y) = 42
    try
        @assert 1 == 2 string("the answer to the ultimate question: ",
                              deepthought(6, 9))
        error("unexpected")
    catch ex
        @test isa(ex, AssertionError)
        @test ex.msg == "the answer to the ultimate question: 42"
    end
end

let # test the process title functions, issue #9957
    oldtitle = Sys.get_process_title()
    Sys.set_process_title("julia0x1")
    @test Sys.get_process_title() == "julia0x1"
    Sys.set_process_title(oldtitle)
    @test Sys.get_process_title() == oldtitle
end

# test GC.enable/disable
@test GC.enable(true)
@test GC.enable(false)
@test GC.enable(false) == false
@test GC.enable(true) == false
@test GC.enable(true)

# PR #10984
let
    redir_err = "redirect_stderr(stdout)"
    exename = Base.julia_cmd()
    script = """
        $redir_err
        module A; f() = 1; end; A.f() = 1
        A.f() = 1
        outer() = (g() = 1; g() = 2; g)
        """
    warning_str = read(`$exename --warn-overwrite=yes --startup-file=no -e $script`, String)
    @test warning_str == """
        WARNING: Method definition f() in module A at none:2 overwritten in module Main on the same line (check for duplicate calls to `include`).
        WARNING: Method definition f() in module Main at none:2 overwritten at none:3.
        WARNING: Method definition g() in module Main at none:4 overwritten on the same line.
        """
    warning_str = read(`$exename --startup-file=no -e $script`, String)
    @test warning_str == """
        WARNING: Method definition g() in module Main at none:4 overwritten on the same line.
        """
end

# Debugging tool: return the current state of the enable_finalizers counter.
get_finalizers_inhibited() = ccall(:jl_gc_get_finalizers_inhibited, Int32, (Ptr{Cvoid},), C_NULL)

# lock / unlock
let l = ReentrantLock()
    @test lock(l) === nothing
    @test islocked(l)
    success = Ref(false)
    @test trylock(l) do
        @test lock(l) do
            success[] = true
            return :foo
        end === :foo
        return :bar
    end === :bar
    @test success[]
    t = @async begin
        @test trylock(l) do
            error("unreachable")
        end === false
    end
    @test get_finalizers_inhibited() == 1
    Base.wait(t)
    @test get_finalizers_inhibited() == 1
    @test unlock(l) === nothing
    @test get_finalizers_inhibited() == 0
    @test_throws ErrorException unlock(l)
end

# Lockable{T, L<:AbstractLock}
using Base: Lockable
let
    @test Base.isexported(Base, :Lockable)
    lockable = Lockable(Dict("foo" => "hello"), ReentrantLock())
    # note field access is non-public
    @test lockable.value["foo"] == "hello"
    @test @lock(lockable, lockable[]["foo"]) == "hello"
    lock(lockable) do d
        @test d["foo"] == "hello"
    end
    lock(lockable) do d
        d["foo"] = "goodbye"
    end
    @test lockable.value["foo"] == "goodbye"
    @lock lockable begin
        @test lockable[]["foo"] == "goodbye"
    end
    l = trylock(lockable)
    try
        @test l
    finally
        unlock(lockable)
    end
    # Test 1-arg constructor
    lockable2 = Lockable(Dict("foo" => "hello"))
    @test lockable2.lock isa ReentrantLock
    @test @lock(lockable2, lockable2[]["foo"]) == "hello"
end

@testset "`show` for ReentrantLock" begin
    l = ReentrantLock()
    @test repr(l) == "ReentrantLock()"
    @test repr("text/plain", l) == "ReentrantLock() (unlocked)"
    @lock l begin
        @test startswith(repr("text/plain", l), "ReentrantLock() (locked by current Task (")
    end
    @test repr("text/plain", l) == "ReentrantLock() (unlocked)"
end

for l in (Threads.SpinLock(), ReentrantLock())
    @test get_finalizers_inhibited() == 0
    @test lock(get_finalizers_inhibited, l) == 1
    @test get_finalizers_inhibited() == 0
    try
        GC.enable_finalizers(false)
        GC.enable_finalizers(false)
        @test get_finalizers_inhibited() == 2
        GC.enable_finalizers(true)
        @test get_finalizers_inhibited() == 1
    finally
        @test get_finalizers_inhibited() == 1
        GC.enable_finalizers(false)
        @test get_finalizers_inhibited() == 2
    end
    @test get_finalizers_inhibited() == 2
    GC.enable_finalizers(true)
    @test get_finalizers_inhibited() == 1
    GC.enable_finalizers(true)
    @test get_finalizers_inhibited() == 0
    if Base.isdebugbuild()
        # Note this warning only exists in debug builds
        @test_warn "WARNING: GC finalizers already enabled on this thread." GC.enable_finalizers(true)
    end

    @test lock(l) === nothing
    @test try unlock(l) finally end === nothing
end

@testset "Semaphore" begin
    sem_size = 2
    n = 100
    s = Base.Semaphore(sem_size)

    # explicit acquire-release form
    clock = Threads.Atomic{Int}(1)
    occupied = Threads.Atomic{Int}(0)
    history = fill!(Vector{Int}(undef, 2n), -1)
    @sync for _ in 1:n
        @async begin
            Base.acquire(s)
            history[Threads.atomic_add!(clock, 1)] = Threads.atomic_add!(occupied, 1) + 1
            sleep(rand(0:0.01:0.1))
            history[Threads.atomic_add!(clock, 1)] = Threads.atomic_sub!(occupied, 1) - 1
            Base.release(s)
        end
    end
    @test all(<=(sem_size), history)
    @test all(>=(0), history)
    @test history[end] == 0

    # do-block syntax
    clock = Threads.Atomic{Int}(1)
    occupied = Threads.Atomic{Int}(0)
    history = fill!(Vector{Int}(undef, 2n), -1)
    @sync for _ in 1:n
        @async begin
            @test Base.acquire(s) do
                history[Threads.atomic_add!(clock, 1)] = Threads.atomic_add!(occupied, 1) + 1
                sleep(rand(0:0.01:0.1))
                history[Threads.atomic_add!(clock, 1)] = Threads.atomic_sub!(occupied, 1) - 1
                return :resultvalue
            end === :resultvalue
        end
    end
    @test all(<=(sem_size), history)
    @test all(>=(0), history)
    @test history[end] == 0

    # macro form
    clock = Threads.Atomic{Int}(1)
    occupied = Threads.Atomic{Int}(0)
    history = fill!(Vector{Int}(undef, 2n), -1)
    @sync for _ in 1:n
        @async begin
            @test Base.@acquire s begin
                history[Threads.atomic_add!(clock, 1)] = Threads.atomic_add!(occupied, 1) + 1
                sleep(rand(0:0.01:0.1))
                history[Threads.atomic_add!(clock, 1)] = Threads.atomic_sub!(occupied, 1) - 1
                return :resultvalue
            end === :resultvalue
        end
    end
    @test all(<=(sem_size), history)
    @test all(>=(0), history)
    @test history[end] == 0
end

# task switching

@noinline function f6597(c)
    t = @async nothing
    finalizer(t -> c[] += 1, t)
    Base.wait(t)
    @test c[] == 0
    Base.wait(t)
    nothing
end
let c = Ref(0),
    t2 = @async (wait(); c[] += 99)
    @test c[] == 0
    f6597(c)
    GC.gc() # this should run the finalizer for t
    @test c[] == 1
    yield()
    @test c[] == 1
    yield(t2)
    @test c[] == 100
end

@test_throws ConcurrencyViolationError("deadlock detected: cannot wait on current task") wait(current_task())

@test_throws ConcurrencyViolationError("Cannot yield to currently running task!") yield(current_task())

# issue #41347
let t = @async 1
    wait(t)
    @test_throws ConcurrencyViolationError yield(t)
end

let t = @async error(42)
    Base._wait(t)
    @test_throws ErrorException("42") yieldto(t)
end

# test that @sync is lexical (PR #27164)

const x27164 = Ref(0)
const c27164 = Base.Event()
do_something_async_27164() = @async(begin wait(c27164); x27164[] = 2; end)

let t = nothing
    @sync begin
        @async (sleep(0.1); x27164[] = 1)
        t = do_something_async_27164()
    end
    @test x27164[] == 1
    notify(c27164)
    fetch(t)
    @test x27164[] == 2
end

# timing macros

# test that they don't introduce global vars
global v11801, t11801, names_before_timing
names_before_timing = names(@__MODULE__, all = true)

let t = @elapsed 1+1
    @test isa(t, Real) && t >= 0
end

let
    stats = @timed sin(1)
    @test stats.value == sin(1)
    @test isa(stats.time, Real) && stats.time >= 0
    @test isa(stats.compile_time, Real) && stats.compile_time >= 0
    @test isa(stats.recompile_time, Real) && stats.recompile_time >= 0
    @test stats.compile_time <= stats.time

    # The return type of gcstats was changed in Julia 1.4 (# 34147)
    # Test that the 1.0 API still works
    val, t, bytes, gctime, gcstats = stats
    @test val === stats.value
    @test t === stats.time
    @test bytes === stats.bytes
    @test gctime === stats.gctime
    @test gcstats === stats.gcstats
end

# problem after #11801 - at global scope
t11801 = @elapsed 1+1
@test isa(t11801,Real) && t11801 >= 0
v11801, t11801 = @timed sin(1)
@test v11801 == sin(1)
@test isa(t11801,Real) && t11801 >= 0

@test names(@__MODULE__, all = true) == names_before_timing

redirect_stdout(devnull) do # suppress time prints

# Accepted @time argument formats
@test @time true
@test @time "message" true
@test @time 1 true
let msg = "message"
    @test @time msg true
end
let foo() = "message"
    @test @time foo() true
end
let foo() = 1
    @test @time foo() true
end

# Accepted @timev argument formats
@test @timev true
@test @timev "message" true
@test @timev 1 true
let msg = "message"
    @test @timev msg true
end
let foo() = "message"
    @test @timev foo() true
end
let foo() = 1
    @test @timev foo() true
end

# this is internal, but used for easy testing
@test sprint(Base.time_print, 1e9) == "  1.000000 seconds"
@test sprint(Base.time_print, 1e9, 111, 0, 222) == "  1.000000 seconds (222 allocations: 111 bytes)"
@test sprint(Base.time_print, 1e9, 111, 0.5e9, 222) == "  1.000000 seconds (222 allocations: 111 bytes, 50.00% gc time)"
@test sprint(Base.time_print, 1e9, 111, 0, 222, 333) == "  1.000000 seconds (222 allocations: 111 bytes, 333 lock conflicts)"
@test sprint(Base.time_print, 1e9, 0, 0, 0, 333) == "  1.000000 seconds (333 lock conflicts)"
@test sprint(Base.time_print, 1e9, 111, 0, 222, 333, 0.25e9) == "  1.000000 seconds (222 allocations: 111 bytes, 333 lock conflicts, 25.00% compilation time)"
@test sprint(Base.time_print, 1e9, 111, 0.5e9, 222, 333, 0.25e9, 0.175e9) == "  1.000000 seconds (222 allocations: 111 bytes, 50.00% gc time, 333 lock conflicts, 25.00% compilation time: 70% of which was recompilation)"

# @showtime
@test @showtime true
let foo() = true
    @test @showtime foo()
end
let foo() = false
    @test (@showtime foo()) == false
end

# PR #39133, ensure that @time evaluates in the same scope
function time_macro_scope()
    try # try/throw/catch bypasses printing
        @time (time_macro_local_var = 1; throw("expected"))
        return time_macro_local_var
    catch ex
        ex === "expected" || rethrow()
    end
end
@test time_macro_scope() == 1

function timev_macro_scope()
    try # try/throw/catch bypasses printing
        @timev (time_macro_local_var = 1; throw("expected"))
        return time_macro_local_var
    catch ex
        ex === "expected" || rethrow()
    end
end
@test timev_macro_scope() == 1

before_comp, before_recomp = Base.cumulative_compile_time_ns() # no need to turn timing on, @time will do that

# exercise concurrent calls to `@time` for reentrant compilation time measurement.
@sync begin
    t1 = @async @time begin
        sleep(2)
        @eval module M ; f(x,y) = x+y ; end
        @eval M.f(2,3)
    end
    t2 = @async begin
        sleep(1)
        @time 2 + 2
    end
end

after_comp, after_recomp = Base.cumulative_compile_time_ns() # no need to turn timing off, @time will do that
@test after_comp >= before_comp;
@test after_recomp >= before_recomp;
@test after_recomp - before_recomp <= after_comp - before_comp;

# should be approximately 60,000,000 ns, we definitely shouldn't exceed 100x that value
# failing this probably means an uninitialized variable somewhere
@test after_comp - before_comp < 6_000_000_000;

end # redirect_stdout

# issue #48024, avoid overcounting timers
begin
    double(x::Real) = 2x;
    calldouble(container) = double(container[1]);
    calldouble2(container) = calldouble(container);

    Base.Experimental.@force_compile;
    local elapsed = Base.time_ns();
    Base.cumulative_compile_timing(true);
    local compiles = Base.cumulative_compile_time_ns();
    @eval calldouble([1.0]);
    Base.cumulative_compile_timing(false);
    compiles = Base.cumulative_compile_time_ns() .- compiles;
    elapsed = Base.time_ns() - elapsed;

    # compile time should be at most total time
    @test compiles[1] <= elapsed
    # recompile time should be at most compile time
    @test compiles[2] <= compiles[1]

    elapsed = Base.time_ns();
    Base.cumulative_compile_timing(true);
    compiles = Base.cumulative_compile_time_ns();
    @eval calldouble(1.0);
    Base.cumulative_compile_timing(false);
    compiles = Base.cumulative_compile_time_ns() .- compiles;
    elapsed = Base.time_ns() - elapsed;

    # compile time should be at most total time
    @test compiles[1] <= elapsed
    # recompile time should be at most compile time
    @test compiles[2] <= compiles[1]
end

macro capture_stdout(ex)
    quote
        mktemp() do fname, f
            redirect_stdout(f) do
                $(esc(ex))
            end
            seekstart(f)
            read(f, String)
        end
    end
end

# issue #48024, but with the time macro itself
begin
    double(x::Real) = 2x;
    calldouble(container) = double(container[1]);
    calldouble2(container) = calldouble(container);

    local first = @capture_stdout @time @eval calldouble([1.0])
    local second = @capture_stdout @time @eval calldouble2(1.0)

    # these functions were not recompiled
    local matches = collect(eachmatch(r"(\d+(?:\.\d+)?)%", first))
    @test length(matches) == 1
    @test parse(Float64, matches[1][1]) > 0.0
    @test parse(Float64, matches[1][1]) <= 100.0

    matches = collect(eachmatch(r"(\d+(?:\.\d+)?)%", second))
    @test length(matches) == 1
    @test parse(Float64, matches[1][1]) > 0.0
    @test parse(Float64, matches[1][1]) <= 100.0
end

# compilation reports in @time, @timev
let f = gensym("f"), callf = gensym("callf"), call2f = gensym("call2f")
    @eval begin
        $f(::Real) = 1
        $callf(container) = $f(container[1])
        $call2f(container) = $callf(container)
        c64 = [1.0]
        c32 = [1.0f0]
        cabs = AbstractFloat[1.0]

        out = @capture_stdout @time $call2f(c64)
        @test occursin("% compilation time", out)
        out = @capture_stdout @time $call2f(c64)
        @test occursin("% compilation time", out) == false

        out = @capture_stdout @time $call2f(c32)
        @test occursin("% compilation time", out)
        out = @capture_stdout @time $call2f(c32)
        @test occursin("% compilation time", out) == false

        out = @capture_stdout @time $call2f(cabs)
        @test occursin("% compilation time", out)
        out = @capture_stdout @time $call2f(cabs)
        @test occursin("% compilation time", out) == false

        $f(::Float64) = 2
        out = @capture_stdout @time $call2f(c64)
        @test occursin("% compilation time:", out)
        @test occursin("% of which was recompilation", out)
    end
end
let f = gensym("f"), callf = gensym("callf"), call2f = gensym("call2f")
    @eval begin
        $f(::Real) = 1
        $callf(container) = $f(container[1])
        $call2f(container) = $callf(container)
        c64 = [1.0]
        c32 = [1.0f0]
        cabs = AbstractFloat[1.0]

        out = @capture_stdout @timev $call2f(c64)
        @test occursin("% compilation time", out)
        out = @capture_stdout @timev $call2f(c64)
        @test occursin("% compilation time", out) == false

        out = @capture_stdout @timev $call2f(c32)
        @test occursin("% compilation time", out)
        out = @capture_stdout @timev $call2f(c32)
        @test occursin("% compilation time", out) == false

        out = @capture_stdout @timev $call2f(cabs)
        @test occursin("% compilation time", out)
        out = @capture_stdout @timev $call2f(cabs)
        @test occursin("% compilation time", out) == false

        $f(::Float64) = 2
        out = @capture_stdout @timev $call2f(c64)
        @test occursin("% compilation time:", out)
        @test occursin("% of which was recompilation", out)
    end
end

# interactive utilities

struct ambigconvert; end # inject a problematic `convert` method to ensure it still works
Base.convert(::Any, v::ambigconvert) = v

import Base.summarysize
@test summarysize(Core) > Base.summarysize(Core.Intrinsics) > Core.sizeof(Core)
@test summarysize(Base) > 100_000 * sizeof(Ptr)

let R = Ref{Any}(nothing), depth = 10^6
    for i = 1:depth
        R = Ref{Any}(R)
    end
    R = Core.svec(R, R)
    @test summarysize(R) == (depth + 4) * sizeof(Ptr)
end

# issue #25367 - summarysize with reshaped arrays
let A = zeros(1000), B = reshape(A, (1,1000))
    @test summarysize((A,B)) < 2 * sizeof(A)

    # check that object header is accounted for
    @test summarysize(A) > sizeof(A)
end

# issue #32881
mutable struct S32881; end
let s = "abc"
    @test summarysize([s,s]) < summarysize(["abc","xyz"])
end
@test summarysize(Vector{Union{Nothing,Missing}}(undef, 16)) < summarysize(Vector{Union{Nothing,Missing}}(undef, 32))
@test summarysize(Vector{Nothing}(undef, 16)) == summarysize(Vector{Nothing}(undef, 32))
@test summarysize(S32881()) == sizeof(Int)

# issue #33675
let vec = vcat(missing, ones(100000))
    @test length(unique(summarysize(vec) for i = 1:20)) == 1
end

# issue #40773
let s = Set(1:100)
    @test summarysize([s]) > summarysize(s)
end

# issue #44780
@test summarysize(BigInt(2)^1000) > summarysize(BigInt(2))

# issue #53061
mutable struct S53061
    x::Union{Float64, Tuple{Float64, Float64}}
    y::Union{Float64, Tuple{Float64, Float64}}
end
let s = S53061[S53061(rand(), (rand(),rand())) for _ in 1:10^4]
    @test allequal(summarysize(s) for i in 1:10)
end
struct Z53061
    x::S53061
    y::Int64
end
let z = Z53061[Z53061(S53061(rand(), (rand(),rand())), 0) for _ in 1:10^4]
    @test allequal(summarysize(z) for i in 1:10)
    # broken on i868 linux. issue #54895
    @test abs(summarysize(z) - 640000)/640000 <= 0.01 broken = Sys.WORD_SIZE == 32 && Sys.islinux()
end

# issue #57506
let len = 100, m1 = Memory{UInt8}(1:len), m2 = Memory{Union{Nothing,UInt8}}(1:len)
    @test summarysize(m2) == summarysize(m1) + len
end

## test conversion from UTF-8 to UTF-16 (for Windows APIs)

# empty arrays
@test transcode(UInt16, UInt8[]) == UInt16[]
@test transcode(UInt8, UInt16[]) == UInt8[]

# UTF-8-like sequences
V8 = [
    # 1-byte (ASCII)
    ([0x00],[0x0000])
    ([0x0a],[0x000a])
    ([0x7f],[0x007f])
    # 2-byte
    ([0xc0,0x80],[0x0000]) # overlong encoding
    ([0xc1,0xbf],[0x007f]) # overlong encoding
    ([0xc2,0x80],[0x0080])
    ([0xc3,0xbf],[0x00ff])
    ([0xc4,0x80],[0x0100])
    ([0xc4,0xa3],[0x0123])
    ([0xdf,0xbf],[0x07ff])
    # 3-byte
    ([0xe0,0x80,0x80],[0x0000]) # overlong encoding
    ([0xe0,0x81,0xbf],[0x007f]) # overlong encoding
    ([0xe0,0x82,0x80],[0x0080]) # overlong encoding
    ([0xe0,0x9f,0xbf],[0x07ff]) # overlong encoding
    ([0xe0,0xa0,0x80],[0x0800])
    ([0xe0,0xa2,0x9a],[0x089a])
    ([0xe1,0x88,0xb4],[0x1234])
    ([0xea,0xaf,0x8d],[0xabcd])
    ([0xed,0x9f,0xbf],[0xd7ff])
    ([0xed,0xa0,0x80],[0xd800]) # invalid code point – high surrogate
    ([0xed,0xaf,0xbf],[0xdbff]) # invalid code point – high surrogate
    ([0xed,0xb0,0x80],[0xdc00]) # invalid code point – low surrogate
    ([0xed,0xbf,0xbf],[0xdfff]) # invalid code point – low surrogate
    ([0xee,0x80,0x80],[0xe000])
    ([0xef,0xbf,0xbf],[0xffff])
    # 4-byte
    ([0xf0,0x80,0x80,0x80],[0x0000]) # overlong encoding
    ([0xf0,0x80,0x81,0xbf],[0x007f]) # overlong encoding
    ([0xf0,0x80,0x82,0x80],[0x0080]) # overlong encoding
    ([0xf0,0x80,0x9f,0xbf],[0x07ff]) # overlong encoding
    ([0xf0,0x80,0xa0,0x80],[0x0800]) # overlong encoding
    ([0xf0,0x8f,0xbf,0xbf],[0xffff]) # overlong encoding
    ([0xf0,0x90,0x80,0x80],[0xd800,0xdc00]) # U+10000
    ([0xf0,0x90,0x8d,0x88],[0xd800,0xdf48]) # U+10348
    ([0xf0,0x90,0x90,0xb7],[0xd801,0xdc37]) # U+10437
    ([0xf0,0xa4,0xad,0xa2],[0xd852,0xdf62]) # U+24b62
    ([0xf2,0xab,0xb3,0x9e],[0xda6f,0xdcde]) # U+abcde
    ([0xf3,0xbf,0xbf,0xbf],[0xdbbf,0xdfff]) # U+fffff
    ([0xf4,0x80,0x80,0x80],[0xdbc0,0xdc00]) # U+100000
    ([0xf4,0x8a,0xaf,0x8d],[0xdbea,0xdfcd]) # U+10abcd
    ([0xf4,0x8f,0xbf,0xbf],[0xdbff,0xdfff]) # U+10ffff
]

# non UTF-8-like sequences
X8 = Vector{UInt8}[
    # invalid 1-byte sequences
    [0x80], # 1 leading ones
    [0xbf],
    [0xc0], # 2 leading ones
    [0xdf],
    [0xe0], # 3 leading ones
    [0xef],
    [0xf0], # 4 leading ones
    [0xf7],
    [0xf8], # 5 leading ones
    [0xfb],
    [0xfc], # 6 leading ones
    [0xfd],
    [0xfe], # 7 leading ones
    [0xff], # 8 leading ones
    # other invalid sequences
    [0xf4,0x90,0xbf,0xbf],
    [0xf4,0x91,0x80,0x80],
    [0xf7,0x80,0x80,0x80],
    [0xf7,0xbf,0xbf,0xbf],
    [0xf8,0x80,0x80,0x80],
    [0xf8,0xbf,0xbf,0xbf],
    [0xff,0x80,0x80,0x80],
    [0xff,0xbf,0xbf,0xbf],
]

for s in [map(first,V8); X8],
    i = 1:length(s)-1,
    j = i+1:length(s)-(i==1)
    ss = s[i:j]
    ss in X8 || push!(X8, ss)
end
sort!(X8, lt=isless)
sort!(X8, by=length)

I8 = [(s,map(UInt16,s)) for s in X8]

for (X,Y,Z) in ((V8,V8,V8), (I8,V8,I8), (V8,I8,V8), (V8,V8,I8), (I8,V8,V8))
    for (a8, a16) in X
        @test transcode(UInt16, a8) == a16
        for (b8, b16) in Y
            ab8 = [a8; b8]
            ab16 = [a16; b16]
            @test transcode(UInt16, ab8) == ab16
            for (c8, c16) in Z
                abc8 = [ab8; c8]
                abc16 = [ab16; c16]
                @test transcode(UInt16, abc8) == abc16
            end
        end
    end
end

# UTF-16-like sequences
V16 = [
    # 1-unit UTF-16, 1-byte UTF-8 (ASCII)
    ([0x0000],[0x00])
    ([0x000a],[0x0a])
    ([0x007f],[0x7f])
    # 1-unit UTF-16, 2-byte UTF-8
    ([0x0080],[0xc2,0x80])
    ([0x00ff],[0xc3,0xbf])
    ([0x0100],[0xc4,0x80])
    ([0x0123],[0xc4,0xa3])
    ([0x07ff],[0xdf,0xbf])
    # 1-unit UTF-16, 3-byte UTF-8
    ([0x0800],[0xe0,0xa0,0x80])
    ([0x089a],[0xe0,0xa2,0x9a])
    ([0x1234],[0xe1,0x88,0xb4])
    ([0xabcd],[0xea,0xaf,0x8d])
    ([0xd7ff],[0xed,0x9f,0xbf])
    ([0xe000],[0xee,0x80,0x80])
    ([0xffff],[0xef,0xbf,0xbf])
    # 2-unit UTF-16, 4-byte UTF-8
    ([0xd800,0xdc00],[0xf0,0x90,0x80,0x80]) # U+10000
    ([0xd800,0xdf48],[0xf0,0x90,0x8d,0x88]) # U+10348
    ([0xd801,0xdc37],[0xf0,0x90,0x90,0xb7]) # U+10437
    ([0xd852,0xdf62],[0xf0,0xa4,0xad,0xa2]) # U+24b62
    ([0xda6f,0xdcde],[0xf2,0xab,0xb3,0x9e]) # U+abcde
    ([0xdbbf,0xdfff],[0xf3,0xbf,0xbf,0xbf]) # U+fffff
    ([0xdbc0,0xdc00],[0xf4,0x80,0x80,0x80]) # U+100000
    ([0xdbea,0xdfcd],[0xf4,0x8a,0xaf,0x8d]) # U+10abcd
    ([0xdbff,0xdfff],[0xf4,0x8f,0xbf,0xbf]) # U+10ffff
]

I16 = [
    ([0xd800],[0xed,0xa0,0x80]) # high surrogate
    ([0xdbff],[0xed,0xaf,0xbf]) # high surrogate
    ([0xdc00],[0xed,0xb0,0x80]) # low surrogate
    ([0xdfff],[0xed,0xbf,0xbf]) # low surrogate
]

for (X,Y,Z) in ((V16,V16,V16), (I16,V16,I16), (V16,I16,V16), (V16,V16,I16), (I16,V16,V16))
    for (a16, a8) in X
        @test transcode(UInt8, a16) == a8
        @test transcode(UInt16, a8) == a16
        for (b16, b8) in Y
            ab16 = [a16; b16]
            ab8 = [a8; b8]
            @test transcode(UInt8, ab16) == ab8
            @test transcode(UInt16, ab8) == ab16
            for (c16, c8) in Z
                abc16 = [ab16; c16]
                abc8 = [ab8; c8]
                @test transcode(UInt8, abc16) == abc8
                @test transcode(UInt16, abc8) == abc16
            end
        end
    end
end

let s = "abcα🐨\0x\0"
    for T in (UInt8, UInt16, UInt32, Int32)
        @test transcode(T, s) == transcode(T, codeunits(s))
        @test transcode(String, transcode(T, s)) == s
    end
end

let X = UInt8[0x30,0x31,0x32]
    for T in (UInt8, UInt16, UInt32, Int32)
        @test transcode(UInt8,transcode(T, X)) == X
        @test transcode(UInt8,transcode(T, 0x30:0x32)) == X
    end
end

let optstring = repr("text/plain", Base.JLOptions())
    @test startswith(optstring, "JLOptions(\n")
    @test !occursin("Ptr{UInt8}", optstring)
    @test endswith(optstring, "\n)")
    @test occursin(" = \"", optstring)
end
let optstring = repr(Base.JLOptions())
    @test startswith(optstring, "JLOptions(")
    @test endswith(optstring, ")")
    @test !occursin("\n", optstring)
    @test !occursin("Ptr{UInt8}", optstring)
    @test occursin(" = \"", optstring)
end

# Base.securezero! functions (#17579)
import Base: securezero!, unsafe_securezero!
let a = [1,2,3]
    @test securezero!(a) === a == [0,0,0]
    a[:] = 1:3
    @test unsafe_securezero!(pointer(a), length(a)) == pointer(a)
    @test a == [0,0,0]
    a[:] = 1:3
    @test unsafe_securezero!(Ptr{Cvoid}(pointer(a)), sizeof(a)) == Ptr{Cvoid}(pointer(a))
    @test a == [0,0,0]
end

# PR #28038 (prompt/getpass stream args)
@test_throws MethodError Base.getpass(IOBuffer(), stdout, "pass")
let buf = IOBuffer()
    @test Base.prompt(IOBuffer("foo\nbar\n"), buf, "baz") == "foo"
    @test String(take!(buf)) == "baz: "
    @test Base.prompt(IOBuffer("\n"), buf, "baz", default="foobar") == "foobar"
    @test String(take!(buf)) == "baz [foobar]: "
    @test Base.prompt(IOBuffer("blah\n"), buf, "baz", default="foobar") == "blah"
end

# these tests are not in a test block so that they will compile separately
@static if Sys.iswindows()
    SetLastError(code) = ccall(:SetLastError, stdcall, Cvoid, (UInt32,), code)
else
    SetLastError(_) = nothing
end
@test Libc.errno(0xc0ffee) === nothing
@test SetLastError(0xc0def00d) === nothing
let finalized = false
    function closefunc(_)
        Libc.errno(0)
        SetLastError(0)
        finalized = true
    end
    @eval (finalizer($closefunc, zeros()); nothing)
    GC.gc(); GC.gc(); GC.gc(); GC.gc()
    @test finalized
end
@static if Sys.iswindows()
    @test ccall(:GetLastError, stdcall, UInt32, ()) == 0xc0def00d
    @test Libc.GetLastError() == 0xc0def00d
end
@test Libc.errno() == 0xc0ffee

# Test that we can VirtualProtect jitted code to writable
@noinline function WeVirtualProtectThisToRWX(x, y)
    return x + y
end
@static if Sys.iswindows()
    let addr = @cfunction(WeVirtualProtectThisToRWX, UInt64, (UInt64, UInt64))
        addr = addr - (UInt64(addr) % 4096)
        PAGE_EXECUTE_READWRITE = 0x40
        oldPerm = Ref{UInt32}()
        err18083 = ccall(:VirtualProtect, stdcall, Cint,
            (Ptr{Cvoid}, Csize_t, UInt32, Ptr{UInt32}),
            addr, 4096, PAGE_EXECUTE_READWRITE, oldPerm)
        err18083 == 0 && Base.windowserror(:VirtualProtect)
    end
end

let buf = IOBuffer()
    printstyled(IOContext(buf, :color=>true), "foo", color=:red)
    @test startswith(String(take!(buf)), Base.text_colors[:red])
end

# Test that `printstyled` accepts non-string values, just as `print` does
let buf_color = IOBuffer()
    args = (3.2, "foo", :testsym)
    printstyled(IOContext(buf_color, :color=>true), args..., color=:red)
    buf_plain = IOBuffer()
    print(buf_plain, args...)
    expected_str = string(Base.text_colors[:red],
                          String(take!(buf_plain)),
                          Base.text_colors[:default])
    @test expected_str == String(take!(buf_color))
end

# Test that `printstyled` on multiline input prints the ANSI codes
# on each line
let buf_color = IOBuffer()
    str = "Two\nlines"
    printstyled(IOContext(buf_color, :color=>true), str; bold=true, color=:red)
    @test String(take!(buf_color)) == "\e[31m\e[1mTwo\e[22m\e[39m\n\e[31m\e[1mlines\e[22m\e[39m"
end

if stdout isa Base.TTY
    @test haskey(stdout, :color) == true
    @test haskey(stdout, :bar) == false
    @test (:color=>Base.have_color) in stdout
    @test (:color=>!Base.have_color) ∉ stdout
    @test stdout[:color] == get(stdout, :color, nothing) == Base.have_color
    @test get(stdout, :bar, nothing) === nothing
    @test_throws KeyError stdout[:bar]
end

@testset "`displaysize` on closed TTY #34620" begin
    Main.FakePTYs.with_fake_pty() do rawfd, _
        tty = open(rawfd)::Base.TTY
        @test displaysize(tty) isa Tuple{Integer,Integer}
        close(tty)
        @test_throws Base.IOError displaysize(tty)
    end
end

let
    global c_18711 = 0
    buf = IOContext(IOBuffer(), :hascontext => true)
    Base.with_output_color(:red, buf) do buf
        global c_18711
        get(buf, :hascontext, false) && (c_18711 += 1)
    end
    @test c_18711 == 1
end

let buf = IOBuffer()
    buf_color = IOContext(buf, :color => true)
    printstyled(buf_color, "foo", color=:red)
    # Check that we get back to normal text color in the end
    @test String(take!(buf)) == "\e[31mfoo\e[39m"

    # Check that boldness is turned off
    printstyled(buf_color, "foo"; bold=true, color=:red)
    @test String(take!(buf)) == "\e[31m\e[1mfoo\e[22m\e[39m"

    # Check that italic is turned off
    printstyled(buf_color, "foo"; italic=true, color=:red)
    @test String(take!(buf)) == "\e[31m\e[3mfoo\e[23m\e[39m"

    # Check that underline is turned off
    printstyled(buf_color, "foo"; color = :red, underline = true)
    @test String(take!(buf)) == "\e[31m\e[4mfoo\e[24m\e[39m"

    # Check that blink is turned off
    printstyled(buf_color, "foo"; color = :red, blink = true)
    @test String(take!(buf)) == "\e[31m\e[5mfoo\e[25m\e[39m"

    # Check that reverse is turned off
    printstyled(buf_color, "foo"; color = :red, reverse = true)
    @test String(take!(buf)) == "\e[31m\e[7mfoo\e[27m\e[39m"

    # Check that hidden is turned off
    printstyled(buf_color, "foo"; color = :red, hidden = true)
    @test String(take!(buf)) == "\e[31m\e[8mfoo\e[28m\e[39m"

    # Check that all options can be turned on simultaneously
    printstyled(buf_color, "foo"; color = :red, bold = true, italic = true, underline = true, blink = true, reverse = true, hidden = true)
    @test String(take!(buf)) == "\e[31m\e[1m\e[3m\e[4m\e[5m\e[7m\e[8mfoo\e[28m\e[27m\e[25m\e[24m\e[22m\e[23m\e[39m"
end

abstract type DA_19281{T, N} <: AbstractArray{T, N} end
Base.convert(::Type{Array{S, N}}, ::DA_19281{T, N}) where {S,T,N} = error()
x_19281 = [(), (1,)]
mutable struct Foo_19281
    f::Vector{Tuple}
    Foo_19281() = new(x_19281)
end

@testset "test this does not segfault #19281" begin
    @test Foo_19281().f[1] == ()
    @test Foo_19281().f[2] == (1,)
end

let
    x_notdefined = Ref{String}()
    @test !isassigned(x_notdefined)

    x_defined = Ref{String}("Test")
    @test isassigned(x_defined)
end

mutable struct Demo_20254
    arr::Array{String}
end

# these cause stack overflows and are a little flaky on CI, ref #20256
if Base.get_bool_env("JULIA_TESTFULL", false)
    function Demo_20254(arr::AbstractArray=Any[])
        Demo_20254(string.(arr))
    end

    _get_19433(x::NTuple{1}) = (something(x[1]),)
    _get_19433(xs::Vararg) = (something(xs[1]), _get_19433(xs[2:end])...)

    f_19433(f_19433, xs...) = f_19433(_get_19433(xs)...)

    @testset "test this does not crash, issue #19433 and #20254" begin
        @test_throws StackOverflowError Demo_20254()
        @test_throws StackOverflowError f_19433(+, 1, 2)
    end
end

# Test issue #19774 invokelatest fix.

# we define this in a module to allow rewriting
# rather than needing an extra eval.
module Issue19774
f(x) = 1
end

# First test the world issue condition.
let foo() = begin
        @eval Issue19774.f(x::Int) = 2
        return Issue19774.f(0)
    end
    @test foo() == 1    # We should be using the original function.
end

# Now check that invokelatest fixes that issue.
let foo() = begin
        @eval Issue19774.f(x::Int) = 3
        return Base.invokelatest(Issue19774.f, 0)
    end
    @test foo() == 3
end

# Check that the kwargs conditions also works
module Kwargs19774
f(x, y; z=0) = x * y + z
end

@test Kwargs19774.f(2, 3; z=1) == 7

let foo() = begin
        @eval Kwargs19774.f(x::Int, y::Int; z=3) = z
        return Base.invokelatest(Kwargs19774.f, 2, 3; z=1)
    end
    @test foo() == 1
end

module atinvokelatest
f(x) = 1
g(x, y; z=0) = x * y + z
mutable struct X; x; end
Base.getproperty(::X, ::Any) = error("overload me")
Base.setproperty!(::X, ::Any, ::Any) = error("overload me")
struct Xs
    xs::Vector{Any}
end
Base.getindex(::Xs, ::Any) = error("overload me")
Base.setindex!(::Xs, ::Any, ::Any) = error("overload me")
end

let call_test() = begin
        @eval atinvokelatest.f(x::Int) = 3
        return @invokelatest atinvokelatest.f(0)
    end
    @test call_test() == 3

    call_with_kws_test() = begin
        @eval atinvokelatest.g(x::Int, y::Int; z=3) = z
        return @invokelatest atinvokelatest.g(2, 3; z=1)
    end
    @test call_with_kws_test() == 1

    getproperty_test() = begin
        @eval Base.getproperty(x::atinvokelatest.X, f::Symbol) = getfield(x, f)
        x = atinvokelatest.X(nothing)
        return @invokelatest x.x
    end
    @test isnothing(getproperty_test())

    setproperty!_test() = begin
        @eval Base.setproperty!(x::atinvokelatest.X, f::Symbol, @nospecialize(v)) = setfield!(x, f, v)
        x = atinvokelatest.X(nothing)
        @invokelatest x.x = 1
        return x
    end
    x = setproperty!_test()
    @test getfield(x, :x) == 1

    getindex_test() = begin
        @eval Base.getindex(xs::atinvokelatest.Xs, idx::Int) = xs.xs[idx]
        xs = atinvokelatest.Xs(Any[nothing])
        return @invokelatest xs[1]
    end
    @test isnothing(getindex_test())

    setindex!_test() = begin
        @eval function Base.setindex!(xs::atinvokelatest.Xs, @nospecialize(v), idx::Int)
            xs.xs[idx] = v
        end
        xs = atinvokelatest.Xs(Any[nothing])
        @invokelatest xs[1] = 1
        return xs
    end
    xs = setindex!_test()
    @test xs.xs[1] == 1
end

abstract type InvokeX end
Base.getproperty(::InvokeX, ::Symbol) = error("overload InvokeX")
Base.setproperty!(::InvokeX, ::Symbol, @nospecialize(v::Any)) = error("overload InvokeX")
mutable struct InvokeX2 <: InvokeX; x; end
Base.getproperty(x::InvokeX2, f::Symbol) = getfield(x, f)
Base.setproperty!(x::InvokeX2, f::Symbol, @nospecialize(v::Any)) = setfield!(x, f, v)

abstract type InvokeXs end
Base.getindex(::InvokeXs, ::Int) = error("overload InvokeXs")
Base.setindex!(::InvokeXs, @nospecialize(v::Any), ::Int) = error("overload InvokeXs")
struct InvokeXs2 <: InvokeXs
    xs::Vector{Any}
end
Base.getindex(xs::InvokeXs2, idx::Int) = xs.xs[idx]
Base.setindex!(xs::InvokeXs2, @nospecialize(v::Any), idx::Int) = xs.xs[idx] = v

@testset "@invoke macro" begin
    # test against `invoke` doc example
    let f(x::Real) = x^2
        f(x::Integer) = 1 + @invoke f(x::Real)
        @test f(2) == 5
    end

    let f1(::Integer) = Integer
        f1(::Real) = Real;
        f2(x::Real) = _f2(x)
        _f2(::Integer) = Integer
        _f2(_) = Real
        @test f1(1) === Integer
        @test f2(1) === Integer
        @test @invoke(f1(1::Real)) === Real
        @test @invoke(f2(1::Real)) === Integer
    end

    # when argument's type annotation is omitted, it should be specified as `Core.Typeof(x)`
    let f(_) = Any
        f(x::Integer) = Integer
        @test f(1) === Integer
        @test @invoke(f(1::Any)) === Any
        @test @invoke(f(1)) === Integer

        😎(x, y) = 1
        😎(x, ::Type{Int}) = 2
        # Without `Core.Typeof`, the first method would be called
        @test @invoke(😎(1, Int)) == 2
    end

    # handle keyword arguments correctly
    let f(a; kw1 = nothing, kw2 = nothing) = a + max(kw1, kw2)
        f(::Integer; kwargs...) = error("don't call me")

        @test_throws Exception f(1; kw1 = 1, kw2 = 2)
        @test 3 == @invoke f(1::Any; kw1 = 1, kw2 = 2)
    end

    # additional syntax test
    let x = InvokeX2(nothing)
        @test_throws "overload InvokeX" @invoke (x::InvokeX).x
        @test isnothing(@invoke x.x)
        @test_throws "overload InvokeX" @invoke (x::InvokeX).x = 42
        @invoke x.x = 42
        @test 42 == x.x

        xs = InvokeXs2(Any[nothing])
        @test_throws "overload InvokeXs" @invoke (xs::InvokeXs)[1]
        @test isnothing(@invoke xs[1])
        @test_throws "overload InvokeXs" @invoke (xs::InvokeXs)[1] = 42
        @invoke xs[1] = 42
        @test 42 == xs.xs[1]
    end
end

# Endian tests
# For now, we only support little endian.
# Add an `Sys.ARCH` test for big endian when/if we add support for that.
# Do **NOT** use `ENDIAN_BOM` to figure out the endianness
# since that's exactly what we want to test.
@test ENDIAN_BOM == 0x04030201
@test ntoh(0x1) == 0x1
@test hton(0x1) == 0x1
@test ltoh(0x1) == 0x1
@test htol(0x1) == 0x1
@test ntoh(0x102) == 0x201
@test hton(0x102) == 0x201
@test ltoh(0x102) == 0x102
@test htol(0x102) == 0x102
@test ntoh(0x1020304) == 0x4030201
@test hton(0x1020304) == 0x4030201
@test ltoh(0x1020304) == 0x1020304
@test htol(0x1020304) == 0x1020304
@test ntoh(0x102030405060708) == 0x807060504030201
@test hton(0x102030405060708) == 0x807060504030201
@test ltoh(0x102030405060708) == 0x102030405060708
@test htol(0x102030405060708) == 0x102030405060708

@testset "inline bug #18735" begin
    @noinline f(n) = n ? error() : Int
    g() = Union{f(true)}
    @test_throws ErrorException g()
end

include("testenv.jl")


let flags = Cmd(filter(a->!occursin("depwarn", a), collect(test_exeflags)))
    local cmd = `$test_exename $flags --depwarn=yes deprecation_exec.jl`
    run(cmd, devnull)
end

# PR #23664, make sure names don't get added to the default `Main` workspace
@test readlines(`$(Base.julia_cmd()) --startup-file=no -e 'foreach(println, names(Main))'`) == ["Base","Core","Main"]

# issue #26310
@test_warn "undeclared at import time" Core.eval(@__MODULE__, :(import .notdefined_26310__))
@test_warn "undeclared at import time" Core.eval(Main,        :(import ........notdefined_26310__))
@test_nowarn Core.eval(Main, :(import .Main))
@test_nowarn Core.eval(Main, :(import ....Main))

# issue #27239
using Base.BinaryPlatforms: HostPlatform, libc
@testset "strftime tests issue #27239" begin
    # change to non-Unicode Korean to test that it is properly transcoded into valid UTF-8
    korloc = ["ko_KR.EUC-KR", "ko_KR.CP949", "ko_KR.949", "Korean_Korea.949"]
    at_least_one_locale_found = false
    withlocales(korloc) do locale
        at_least_one_locale_found = true
        # Test both the default format and a custom formatting string
        for s in (Libc.strftime(0.0), Libc.strftime("%a %A %b %B %p %Z", 0))
            # Ensure that we always get valid UTF-8 back
            @test isvalid(s)

            # On `musl` it is impossible for `setlocale` to fail, it just falls back to
            # the default system locale, which on our buildbots is en_US.UTF-8.  We'll
            # assert that what we get does _not_ start with `Thu`, as that's what all
            # en_US.UTF-8 encodings would start with.
            # X-ref: https://musl.openwall.narkive.com/kO1vpTWJ/setlocale-behavior-with-missing-locales
            @test !startswith(s, "Thu") broken=(libc(HostPlatform()) == "musl")
        end
    end
    if !at_least_one_locale_found
        @warn "skipping stftime tests: no locale found for testing"
    end
end


using Base: @kwdef

@kwdef struct Test27970Typed
    a::Int
    b::String = "hi"
end

@kwdef struct Test27970Untyped
    a
end

@kwdef struct Test27970Empty end

@testset "No default values in @kwdef" begin
    @test Test27970Typed(a=1) == Test27970Typed(1, "hi")
    # Implicit type conversion (no assertion on kwarg)
    @test Test27970Typed(a=0x03) == Test27970Typed(3, "hi")
    @test_throws UndefKeywordError Test27970Typed()

    @test Test27970Untyped(a=1) == Test27970Untyped(1)
    @test_throws UndefKeywordError Test27970Untyped()

    # Just checking that this doesn't stack overflow on construction
    @test Test27970Empty() == Test27970Empty()
end

abstract type AbstractTest29307 end
@kwdef struct Test29307{T<:Integer} <: AbstractTest29307
    a::T=2
end

@testset "subtyped @kwdef" begin
    @test Test29307() == Test29307{Int}(2)
    @test Test29307(a=0x03) == Test29307{UInt8}(0x03)
    @test Test29307{UInt32}() == Test29307{UInt32}(2)
    @test Test29307{UInt32}(a=0x03) == Test29307{UInt32}(0x03)
end

@kwdef struct TestInnerConstructor
    a = 1
    TestInnerConstructor(a::Int) = (@assert a>0; new(a))
    function TestInnerConstructor(a::String)
        @assert length(a) > 0
        new(a)
    end
end

@testset "@kwdef inner constructor" begin
    @test TestInnerConstructor() == TestInnerConstructor(1)
    @test TestInnerConstructor(a=2) == TestInnerConstructor(2)
    @test_throws AssertionError TestInnerConstructor(a=0)
    @test TestInnerConstructor(a="2") == TestInnerConstructor("2")
    @test_throws AssertionError TestInnerConstructor(a="")
end

const outsidevar = 7
@kwdef struct TestOutsideVar
    a::Int=outsidevar
end
@test TestOutsideVar() == TestOutsideVar(7)

@kwdef mutable struct Test_kwdef_const_atomic
    a
    b::Int
    c::Int = 1
    const d
    const e::Int
    const f = 1
    const g::Int = 1
    @atomic h::Int
end

@testset "const and @atomic fields in @kwdef" begin
    x = Test_kwdef_const_atomic(a = 1, b = 1, d = 1, e = 1, h = 1)
    for f in fieldnames(Test_kwdef_const_atomic)
        @test getfield(x, f) == 1
    end
    @testset "const fields" begin
        @test_throws ErrorException x.d = 2
        @test_throws ErrorException x.e = 2
        @test_throws MethodError x.e = "2"
        @test_throws ErrorException x.f = 2
        @test_throws ErrorException x.g = 2
    end
    @testset "atomic fields" begin
        @test_throws ConcurrencyViolationError x.h = 1
        @atomic x.h = 1
        @test @atomic(x.h) == 1
        @atomic x.h = 2
        @test @atomic(x.h) == 2
    end
end

@kwdef struct Test_kwdef_lineinfo
    a::String
end
@testset "@kwdef constructor line info" begin
    for method in methods(Test_kwdef_lineinfo)
        @test method.file === Symbol(@__FILE__)
        @test ((@__LINE__)-6) ≤ method.line ≤ ((@__LINE__)-5)
    end
end
@kwdef struct Test_kwdef_lineinfo_sparam{S<:AbstractString}
    a::S
end
@testset "@kwdef constructor line info with static parameter" begin
    for method in methods(Test_kwdef_lineinfo_sparam)
        @test method.file === Symbol(@__FILE__)
        @test ((@__LINE__)-6) ≤ method.line ≤ ((@__LINE__)-5)
    end
end

module KwdefWithEsc
    const Int1 = Int
    const val1 = 42
    macro define_struct()
        quote
            @kwdef struct $(esc(:Struct))
                a
                b = val1
                c::Int1
                d::Int1 = val1

                $(esc(quote
                    e
                    f = val2
                    g::Int2
                    h::Int2 = val2
                end))

                $(esc(:(i = val2)))
                $(esc(:(j::Int2)))
                $(esc(:(k::Int2 = val2)))

                l::$(esc(:Int2))
                m::$(esc(:Int2)) = val1

                n = $(esc(:val2))
                o::Int1 = $(esc(:val2))

                $(esc(:p))
                $(esc(:q)) = val1
                $(esc(:s))::Int1
                $(esc(:t))::Int1 = val1
            end
        end
    end
end

module KwdefWithEsc_TestModule
    using ..KwdefWithEsc
    const Int2 = Int
    const val2 = 42
    KwdefWithEsc.@define_struct()
end
@test isdefined(KwdefWithEsc_TestModule, :Struct)

@testset "exports of modules" begin
    @testset "$mod" for (_, mod) in Base.loaded_modules
        mod === Main && continue # Main exports everything
        @testset "$v" for v in names(mod)
            isdefined(mod, v) || @error "missing $v in $mod"
            @test isdefined(mod, v)
        end
    end
end

@testset "ordering UUIDs" begin
    a = Base.UUID("dbd321ed-e87e-4f33-9511-65b7d01cdd55")
    b = Base.UUID("2832b20a-2ad5-46e9-abb1-2d20c8c31dd3")
    @test isless(b, a)
    @test sort([a, b]) == [b, a]
end

@testset "UUID display" begin
    a = Base.UUID("dbd321ed-e87e-4f33-9511-65b7d01cdd55")
    @test repr(a) == "$(Base.UUID)(\"dbd321ed-e87e-4f33-9511-65b7d01cdd55\")"
end

@testset "Libc.rand" begin
    low, high = extrema(Libc.rand(Float64) for i=1:10^4)
    # these fail with probability 2^(-10^4) ≈ 5e-3011
    @test 0 ≤ low < 0.5
    @test 0.5 < high < 1
end

# Pointer 0-arg constructor
@test Ptr{Cvoid}() == C_NULL

@testset "Pointer to unsigned/signed integer" begin
    # assuming UInt and Ptr have the same size
    @assert sizeof(UInt) == sizeof(Ptr{Nothing})
    uint = UInt(0x12345678)
    sint = signed(uint)
    ptr = reinterpret(Ptr{Nothing}, uint)
    @test unsigned(ptr) === uint
    @test signed(ptr) === sint
end

# Finalizer with immutable should throw
@test_throws ErrorException finalizer(x->nothing, 1)
@test_throws ErrorException finalizer(C_NULL, 1)

# FIXME: Issue #57103 Test is specific to Stock GC
@static if Base.USING_STOCK_GC
@testset "GC utilities" begin
    GC.gc()
    GC.gc(true); GC.gc(false)

    GC.safepoint()

    mktemp() do tmppath, _
        open(tmppath, "w") do tmpio
            redirect_stderr(tmpio) do
                GC.enable_logging(true)
                @test GC.logging_enabled()
                GC.gc()
                GC.enable_logging(false)
                @test !GC.logging_enabled()
            end
        end
        @test occursin("GC: pause", read(tmppath, String))
    end
end
end

@testset "fieldtypes Module" begin
    @test fieldtypes(Module) === ()
end


@testset "issue #28188" begin
    @test `$(@__FILE__)` == let file = @__FILE__; `$file` end
end

# Test that read fault on a prot-none region does not incorrectly give
# ReadOnlyMemoryError, but rather crashes the program
const MAP_ANONYMOUS_PRIVATE = Sys.isbsd() ? 0x1002 : 0x22
let script = """
        let ptr = Ptr{Cint}(ccall(:jl_mmap, Ptr{Cvoid},
                                  (Ptr{Cvoid}, Csize_t, Cint, Cint, Cint, Int),
                                  C_NULL, 16*1024, 0, $MAP_ANONYMOUS_PRIVATE, -1, 0))
            try
                unsafe_load(ptr)
            catch e
                println(e)
            end
        end
    """
    cmd = if Sys.isunix()
        # Set the maximum core dump size to 0 to keep this expected crash from
        # producing a (and potentially overwriting an existing) core dump file
        `sh -c "ulimit -c 0; $(Base.shell_escape(Base.julia_cmd())) -e $(Base.shell_escape(script))"`
    else
        `$(Base.julia_cmd()) -e $script`
    end
    p = run(ignorestatus(cmd), devnull, stdout, devnull)
    if p.termsignal == 0
        Sys.isunix() ? @test(p.exitcode ∈ (128+7, 128+10, 128+11)) : @test(p.exitcode != 0) # expect SIGBUS (7 on BSDs or 10 on Linux) or SIGSEGV (11)
    else
        @test(p.termsignal ∈ (7, 10, 11))
    end
end

# issue #41656
run(`$(Base.julia_cmd()) -e 'isempty(x) = true'`)

@testset "Base/timing.jl" begin
    @test Base.jit_total_bytes() >= 0

    # sanity check `@allocations` returns what we expect in some very simple cases.
    # These are inside functions because `@allocations` uses `Experimental.@force_compile`
    # so can be affected by other code in the same scope.
    @test (() -> @allocations "a")() == 0
    @test (() -> @allocations "a" * "b")() == 0 # constant propagation
    @test (() -> @allocations "a" * Base.inferencebarrier("b"))() == 1

    _lock_conflicts, _nthreads = eval(Meta.parse(read(`$(Base.julia_cmd()) -tauto -E '
        _lock_conflicts = @lock_conflicts begin
            l = ReentrantLock()
            Threads.@threads for i in 1:Threads.nthreads()
                 lock(l) do
                    sleep(1)
                end
            end
        end
        _lock_conflicts,Threads.nthreads()
    '`, String)))
    @test _lock_conflicts > 0 skip=(_nthreads < 2) # can only test if the worker can multithread
end

#TODO: merge with `@testset "Base/timing.jl"` once https://github.com/JuliaLang/julia/issues/52948 is resolved
@testset "Base/timing.jl2" begin
    # Test the output of `format_bytes()`
    inputs = [(factor * (Int64(1000)^e),binary) for binary in (false,true), factor in (1,2), e in 0:6][:]
    expected_output = ["1 byte", "1 byte", "2 bytes", "2 bytes", "1000 bytes", "1000 bytes", "2.000 kB", "1.953 KiB",
                        "1000.000 kB", "976.562 KiB", "2.000 MB", "1.907 MiB", "1000.000 MB", "953.674 MiB",
                        "2.000 GB", "1.863 GiB", "1000.000 GB", "931.323 GiB", "2.000 TB", "1.819 TiB",
                        "1000.000 TB", "909.495 TiB", "2.000 PB", "1.776 PiB", "1000.000 PB", "888.178 PiB",
                        "2000.000 PB", "1776.357 PiB"]

    for ((n, binary), expected) in zip(inputs, expected_output)
        @test Base.format_bytes(n; binary) == expected
    end
end

@testset "in_finalizer" begin
    @test !GC.in_finalizer()

    in_fin = Ref{Any}()
    wait(@async begin
        r = Ref(1)
        finalizer(r) do _
            in_fin[] = GC.in_finalizer()
        end
        nothing
    end)
    GC.gc(true); yield()
    @test in_fin[]
end

@testset "Base docstrings" begin
    undoc = Docs.undocumented_names(Base)
    @test_broken isempty(undoc)
    @test isempty(setdiff(undoc, [:BufferStream, :CanonicalIndexError, :CapturedException, :Filesystem, :IOServer, :InvalidStateException, :Order, :PipeEndpoint, :ScopedValues, :Sort, :TTY, :AtomicMemoryRef, :Exception, :GenericMemoryRef, :GlobalRef, :IO, :LineNumberNode, :MemoryRef, :Method, :SegmentationFault, :TypeVar, :arrayref, :arrayset, :arraysize, :const_arrayref]))
end

exported_names(m) = filter(s -> Base.isexported(m, s), names(m))
@testset "Base re-exports Core" begin
    @test issubset(exported_names(Core), exported_names(Base))
end

@testset "Base.Libc docstrings" begin
    @test isempty(Docs.undocumented_names(Libc))
end

@testset "Silenced missed transformations" begin
    # Ensure the WarnMissedTransformationsPass is not on by default
    src = """
        @noinline iteration(i) = (@show(i); return nothing)
        @eval function loop_unroll_full_fail(N)
            for i in 1:N
              iteration(i)
              \$(Expr(:loopinfo, (Symbol("llvm.loop.unroll.full"), 1)))
          end
       end
       loop_unroll_full_fail(3)
    """
    out_err = mktemp() do _, f
        run(`$(Base.julia_cmd()) -e "$src"`, devnull, devnull, f)
        seekstart(f)
        read(f, String)
    end
    @test !occursin("loop not unrolled", out_err)
end

let errs = IOBuffer()
    run(`$(Base.julia_cmd()) -e '
        using Test
        @test isdefined(DataType.name.mt, :backedges)
        Base.Experimental.disable_new_worlds()
        @test_throws "disable_new_worlds" @eval f() = 1
        @test !isdefined(DataType.name.mt, :backedges)
        @test_throws "disable_new_worlds" Base.delete_method(which(+, (Int, Int)))
        @test 1+1 == 2
        using Dates
        '`, devnull, stdout, errs)
    @test occursin("disable_new_worlds", String(take!(errs)))
end

@testset "`@constprop`, `@assume_effects` handling of an unknown setting" begin
    for x ∈ ("constprop", "assume_effects")
        try
            eval(Meta.parse("Base.@$x :unknown f() = 3"))
            error("unexpectedly reached")
        catch e
            e::LoadError
            @test e.error isa ArgumentError
        end
    end
end
