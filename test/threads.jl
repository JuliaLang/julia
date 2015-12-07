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
