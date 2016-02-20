# This file is a part of Julia. License is MIT: http://julialang.org/license

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
    # Next test checks that al loop iterations ran,
    # and were unique (via pigeon-hole principle).
    @test findfirst(found,false) == 0
    if was_inorder
        println(STDERR, "Warning: threaded loop executed in order")
    end
end

test_threaded_loop_and_atomic_add()

# Helper for test_threaded_atomic_minmax that verifies sequential consistency.
function check_minmax_consistency{T}(old::Array{T,1}, m::T, start::T, o::Base.Ordering)
    for v in old
        if v != start
            # Check that atomic op that installed v reported consistent old value.
            @test Base.lt(o, old[v-m+1], v)
        end
    end
end

function test_threaded_atomic_minmax{T}(m::T,n::T)
    mid = m + (n-m)>>1
    x = Atomic{T}(mid)
    y = Atomic{T}(mid)
    oldx = Array(T,n-m+1)
    oldy = Array(T,n-m+1)
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

function threaded_add_locked{LockT}(::Type{LockT}, x, n)
    lock = LockT()
    @threads for i = 1:n
        lock!(lock)
        x = x + 1
        unlock!(lock)
    end
    return x
end

@test threaded_add_locked(SpinLock, 0, 10000) == 10000
@test threaded_add_locked(Threads.RecursiveSpinLock, 0, 10000) == 10000
@test threaded_add_locked(Mutex, 0, 10000) == 10000

# Check if the recursive lock can be locked and unlocked correctly.
let lock = Threads.RecursiveSpinLock()
    @test lock!(lock) == 0
    @test lock!(lock) == 0
    @test unlock!(lock) == 0
    @test unlock!(lock) == 0
    @test unlock!(lock) == 1
end

# Make sure doing a GC while holding a lock doesn't cause dead lock
# PR 14190. (This is only meaningful for threading)
function threaded_gc_locked{LockT}(::Type{LockT})
    lock = LockT()
    @threads for i = 1:20
        lock!(lock)
        gc(false)
        unlock!(lock)
    end
end

threaded_gc_locked(SpinLock)
threaded_gc_locked(Threads.RecursiveSpinLock)
threaded_gc_locked(Mutex)

# Issue 14726
# Make sure that eval'ing in a different module doesn't mess up other threads
orig_curmodule14726 = current_module()
main_var14726 = 1
module M14726
module_var14726 = 1
end

@threads for i in 1:100
    for j in 1:100
        eval(M14726, :(module_var14726 = $j))
    end
end
@test isdefined(:orig_curmodule14726)
@test isdefined(:main_var14726)
@test current_module() == orig_curmodule14726

@threads for i in 1:100
    # Make sure current module is not null.
    # The @test might not be particularly meaningful currently since the
    # thread infrastructures swallows the error. (Same below)
    @test current_module() == orig_curmodule14726
end

module M14726_2
using Base.Test
using Base.Threads
@threads for i in 1:100
    # Make sure current module is the same with the one on the thread that
    # pushes the work onto the threads.
    # The @test might not be particularly meaningful currently since the
    # thread infrastructures swallows the error. (See also above)
    @test current_module() == M14726_2
end
end

# Ensure only LLVM-supported types can be atomic
@test_throws TypeError Atomic{Bool}
@test_throws TypeError Atomic{BigInt}
@test_throws TypeError Atomic{Float64}

# Test atomic memory ordering with load/store
type CommBuf
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
type Peterson
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
