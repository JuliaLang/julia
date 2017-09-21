# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Test
using Base.Threads

# threading constructs

# parallel loop with parallel atomic addition
function threaded_loop(a, r, x)
    @threads for i in r
        a[i] = 1 + atomic_add!(x, 1)
    end
end

function test_threaded_loop_and_atomic_add()
    x = Atomic()
    a = zeros(Int,10000)
    threaded_loop(a,1:10000,x)
    found = zeros(Bool,10000)
    was_inorder = true
    for i=1:length(a)
        was_inorder &= a[i]==i
        found[a[i]] = true
    end
    @test x[] == 10000
    # Next test checks that all loop iterations ran,
    # and were unique (via pigeon-hole principle).
    @test !(false in found)
    if was_inorder
        println(STDERR, "Warning: threaded loop executed in order")
    end
end

test_threaded_loop_and_atomic_add()

# Helper for test_threaded_atomic_minmax that verifies sequential consistency.
function check_minmax_consistency(old::Array{T,1}, m::T, start::T, o::Base.Ordering) where T
    for v in old
        if v != start
            # Check that atomic op that installed v reported consistent old value.
            @test Base.lt(o, old[v-m+1], v)
        end
    end
end

function test_threaded_atomic_minmax(m::T,n::T) where T
    mid = m + (n-m)>>1
    x = Atomic{T}(mid)
    y = Atomic{T}(mid)
    oldx = Array{T}(n-m+1)
    oldy = Array{T}(n-m+1)
    @threads for i = m:n
        oldx[i-m+1] = atomic_min!(x, T(i))
        oldy[i-m+1] = atomic_max!(y, T(i))
    end
    @test x[] == m
    @test y[] == n
    check_minmax_consistency(oldy,m,mid,Base.Forward)
    check_minmax_consistency(oldx,m,mid,Base.Reverse)
end

# The ranges below verify that the correct signed/unsigned comparison is used.
test_threaded_atomic_minmax(Int16(-5000),Int16(5000))
test_threaded_atomic_minmax(UInt16(27000),UInt16(37000))

function threaded_add_locked(::Type{LockT}, x, n) where LockT
    critical = LockT()
    @threads for i = 1:n
        @test lock(critical) === nothing
        @test islocked(critical)
        x = x + 1
        @test unlock(critical) === nothing
    end
    @test !islocked(critical)
    nentered = 0
    nfailed = Atomic()
    @threads for i = 1:n
        if trylock(critical)
            @test islocked(critical)
            nentered += 1
            @test unlock(critical) === nothing
        else
            atomic_add!(nfailed, 1)
        end
    end
    @test 0 < nentered <= n
    @test nentered + nfailed[] == n
    @test !islocked(critical)
    return x
end

@test threaded_add_locked(SpinLock, 0, 10000) == 10000
@test threaded_add_locked(RecursiveSpinLock, 0, 10000) == 10000
@test threaded_add_locked(Mutex, 0, 10000) == 10000

# Check if the recursive lock can be locked and unlocked correctly.
let critical = RecursiveSpinLock()
    @test !islocked(critical)
    @test_throws AssertionError unlock(critical)
    @test lock(critical) === nothing
    @test islocked(critical)
    @test lock(critical) === nothing
    @test trylock(critical) == true
    @test islocked(critical)
    @test unlock(critical) === nothing
    @test islocked(critical)
    @test unlock(critical) === nothing
    @test islocked(critical)
    @test unlock(critical) === nothing
    @test !islocked(critical)
    @test_throws AssertionError unlock(critical)
    @test trylock(critical) == true
    @test islocked(critical)
    @test unlock(critical) === nothing
    @test !islocked(critical)
    @test_throws AssertionError unlock(critical)
    @test !islocked(critical)
end

# Make sure doing a GC while holding a lock doesn't cause dead lock
# PR 14190. (This is only meaningful for threading)
function threaded_gc_locked(::Type{LockT}) where LockT
    critical = LockT()
    @threads for i = 1:20
        @test lock(critical) === nothing
        @test islocked(critical)
        gc(false)
        @test unlock(critical) === nothing
    end
    @test !islocked(critical)
end

threaded_gc_locked(SpinLock)
threaded_gc_locked(Threads.RecursiveSpinLock)
threaded_gc_locked(Mutex)

# Issue 14726
# Make sure that eval'ing in a different module doesn't mess up other threads
orig_curmodule14726 = @__MODULE__
main_var14726 = 1
module M14726
module_var14726 = 1
end

@threads for i in 1:100
    for j in 1:100
        @eval M14726 module_var14726 = $j
    end
end
@test @isdefined(orig_curmodule14726)
@test @isdefined(main_var14726)
@test @__MODULE__() == orig_curmodule14726

@threads for i in 1:100
    # Make sure current module is not null.
    # The @test might not be particularly meaningful currently since the
    # thread infrastructures swallows the error. (Same below)
    @test @__MODULE__() == orig_curmodule14726
end

module M14726_2
using Base.Test
using Base.Threads
@threads for i in 1:100
    # Make sure current module is the same as the one on the thread that
    # pushes the work onto the threads.
    # The @test might not be particularly meaningful currently since the
    # thread infrastructures swallows the error. (See also above)
    @test @__MODULE__() == M14726_2
end
end

# Ensure only LLVM-supported types can be atomic
@test_throws TypeError Atomic{Bool}
@test_throws TypeError Atomic{BigInt}
@test_throws TypeError Atomic{Complex128}

# Test atomic memory ordering with load/store
mutable struct CommBuf
    var1::Atomic{Int}
    var2::Atomic{Int}
    correct_write::Bool
    correct_read::Bool
    CommBuf() = new(Atomic{Int}(0), Atomic{Int}(0), false, false)
end
function test_atomic_write(commbuf::CommBuf, n::Int)
    for i in 1:n
        # The atomic stores guarantee that var1 >= var2
        commbuf.var1[] = i
        commbuf.var2[] = i
    end
    commbuf.correct_write = true
end
function test_atomic_read(commbuf::CommBuf, n::Int)
    correct = true
    while true
        # load var2 before var1
        var2 = commbuf.var2[]
        var1 = commbuf.var1[]
        correct &= var1 >= var2
        var1 == n && break
        # Temporary solution before we have gc transition support in codegen.
        ccall(:jl_gc_safepoint, Void, ())
    end
    commbuf.correct_read = correct
end
function test_atomic()
    commbuf = CommBuf()
    count = 1_000_000
    @threads for i in 1:2
        if i==1
            test_atomic_write(commbuf, count)
        else
            test_atomic_read(commbuf, count)
        end
    end
    @test commbuf.correct_write == true
    @test commbuf.correct_read == true
end
test_atomic()

# Test ordering with fences using Peterson's algorithm
# Example adapted from <https://en.wikipedia.org/wiki/Peterson%27s_algorithm>
mutable struct Peterson
    # State for Peterson's algorithm
    flag::Vector{Atomic{Int}}
    turn::Atomic{Int}
    # Collision detection
    critical::Vector{Atomic{Int}}
    correct::Vector{Bool}
    Peterson() =
        new([Atomic{Int}(0), Atomic{Int}(0)],
            Atomic{Int}(0),
            [Atomic{Int}(0), Atomic{Int}(0)],
            [false, false])
end
function test_fence(p::Peterson, id::Int, n::Int)
    @assert id == mod1(id,2)
    correct = true
    otherid = mod1(id+1,2)
    for i in 1:n
        p.flag[id][] = 1
        p.turn[] = otherid
        atomic_fence()
        while p.flag[otherid][] != 0 && p.turn[] == otherid
            # busy wait
            # Temporary solution before we have gc transition support in codegen.
            ccall(:jl_gc_safepoint, Void, ())
        end
        # critical section
        p.critical[id][] = 1
        correct &= p.critical[otherid][] == 0
        p.critical[id][] = 0
        # end of critical section
        p.flag[id][] = 0
    end
    p.correct[id] = correct
end
function test_fence()
    commbuf = Peterson()
    count = 1_000_000
    @threads for i in 1:2
        test_fence(commbuf, i, count)
    end
    @test commbuf.correct[1] == true
    @test commbuf.correct[2] == true
end
test_fence()

# Test load / store with various types
let atomic_types = [Int8, Int16, Int32, Int64, Int128,
                    UInt8, UInt16, UInt32, UInt64, UInt128,
                    Float16, Float32, Float64]
    # Temporarily omit 128-bit types on 32bit x86
    # 128-bit atomics do not exist on AArch32.
    # And we don't support them yet on power, because they are lowered
    # to `__sync_lock_test_and_set_16`.
    if Sys.ARCH === :i686 || startswith(string(Sys.ARCH), "arm") ||
       Sys.ARCH === :powerpc64le || Sys.ARCH === :ppc64le
        filter!(T -> sizeof(T)<=8, atomic_types)
    end
    for T in atomic_types
        var = Atomic{T}()
        var[] = 42
        @test var[] === T(42)
        old = atomic_xchg!(var, T(13))
        @test old === T(42)
        @test var[] === T(13)
        old = atomic_cas!(var, T(13), T(14))   # this will succeed
        @test old === T(13)
        @test var[] === T(14)
        old = atomic_cas!(var, T(13), T(15))   # this will fail
        @test old === T(14)
        @test var[] === T(14)
    end
end

# Test atomic_cas! and atomic_xchg!
function test_atomic_cas!(var::Atomic{T}, range::StepRange{Int,Int}) where T
    for i in range
        while true
            old = atomic_cas!(var, T(i-1), T(i))
            old == T(i-1) && break
            # Temporary solution before we have gc transition support in codegen.
            ccall(:jl_gc_safepoint, Void, ())
        end
    end
end
for T in (Int32, Int64, Float32, Float64)
    var = Atomic{T}()
    nloops = 1000
    di = nthreads()
    @threads for i in 1:di
        test_atomic_cas!(var, i:di:nloops)
    end
    @test var[] === T(nloops)
end

function test_atomic_xchg!(var::Atomic{T}, i::Int, accum::Atomic{Int}) where T
    old = atomic_xchg!(var, T(i))
    atomic_add!(accum, Int(old))
end
for T in (Int32, Int64, Float32, Float64)
    accum = Atomic{Int}()
    var = Atomic{T}()
    nloops = 1000
    @threads for i in 1:nloops
        test_atomic_xchg!(var, i, accum)
    end
    @test accum[] + Int(var[]) === sum(0:nloops)
end

function test_atomic_float(varadd::Atomic{T}, varmax::Atomic{T}, varmin::Atomic{T}, i::Int) where T
    atomic_add!(varadd, T(i))
    atomic_max!(varmax, T(i))
    atomic_min!(varmin, T(i))
end
for T in (Int32, Int64, Float32, Float64)
    varadd = Atomic{T}()
    varmax = Atomic{T}()
    varmin = Atomic{T}()
    nloops = 1000
    @threads for i in 1:nloops
        test_atomic_float(varadd, varmax, varmin, i)
    end
    @test varadd[] === T(sum(1:nloops))
    @test varmax[] === T(maximum(1:nloops))
    @test varmin[] === T(0)
end
for period in (0.06, Dates.Millisecond(60))
    let async = Base.AsyncCondition(), t
        c = Condition()
        task = schedule(Task(function()
            notify(c)
            wait(c)
            t = Timer(period)
            wait(t)
            ccall(:uv_async_send, Void, (Ptr{Void},), async)
            ccall(:uv_async_send, Void, (Ptr{Void},), async)
            wait(c)
            sleep(period)
            ccall(:uv_async_send, Void, (Ptr{Void},), async)
            ccall(:uv_async_send, Void, (Ptr{Void},), async)
        end))
        wait(c)
        notify(c)
        delay1 = @elapsed wait(async)
        notify(c)
        delay2 = @elapsed wait(async)
        @test istaskdone(task)
        @test delay1 > 0.05
        @test delay2 > 0.05
        @test isopen(async)
        @test !isopen(t)
        close(t)
        close(async)
        @test_throws EOFError wait(async)
        @test !isopen(async)
        @test_throws EOFError wait(t)
        @test_throws EOFError wait(async)
    end
end

complex_cfunction = function(a)
    s = zero(eltype(a))
    @inbounds @simd for i in a
        s += muladd(a[i], a[i], -2)
    end
    return s
end
function test_thread_cfunction()
    @threads for i in 1:1000
        # Make sure this is not inferrable
        # and a runtime call to `jl_function_ptr` will be created
        ccall(:jl_function_ptr, Ptr{Void}, (Any, Any, Any),
              complex_cfunction, Float64, Tuple{Ref{Vector{Float64}}})
    end
end
test_thread_cfunction()

# Compare the two ways of checking if threading is enabled.
# `jl_tls_states` should only be defined on non-threading build.
if ccall(:jl_threading_enabled, Cint, ()) == 0
    @test nthreads() == 1
    cglobal(:jl_tls_states) != C_NULL
else
    @test_throws ErrorException cglobal(:jl_tls_states)
end

function test_thread_range()
    a = zeros(Int, nthreads())
    @threads for i in 1:threadid()
        a[i] = 1
    end
    for i in 1:threadid()
        @test a[i] == 1
    end
    for i in (threadid() + 1):nthreads()
        @test a[i] == 0
    end
end
test_thread_range()

# Thread safety of `jl_load_and_lookup`.
function test_load_and_lookup_18020(n)
    @threads for i in 1:n
        try
            ccall(:jl_load_and_lookup,
                  Ptr{Void}, (Cstring, Cstring, Ref{Ptr{Void}}),
                  "$i", :f, C_NULL)
        end
    end
end
test_load_and_lookup_18020(10000)

# Nested threaded loops
# This may not be efficient/fully supported but should work without crashing.....
function test_nested_loops()
    a = zeros(Int, 100, 100)
    @threads for i in 1:100
        @threads for j in 1:100
            a[j, i] = i + j
        end
    end
    for i in 1:100
        for j in 1:100
            @test a[j, i] == i + j
        end
    end
end
test_nested_loops()
