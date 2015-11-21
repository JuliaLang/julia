using Base.Test
using Base.Threads

# threading constructs

# parallel call form
function threaded_call(A)
    tid = threadid()
    A[tid] = tid
end

function test_threaded_call()
    expected = collect(1:nthreads())
    arr = zeros(Int16, nthreads())
    @threads all threaded_call(arr)
    @test arr == expected
end

test_threaded_call()

# parallel loop form
function threaded_loop(A)
    @threads all for i = 1:nthreads()
        tid = threadid()
        A[i] = tid
    end
end

function test_threaded_loop()
    expected = collect(1:nthreads())
    arr = zeros(Int16, nthreads())
    threaded_loop(arr)
    @test arr == expected
end

test_threaded_loop()

# parallel block form
function threaded_block(A)
    @threads all begin
        tid = threadid()
        A[tid] = tid
    end
end

function test_threaded_block()
    expected = collect(1:nthreads())
    arr = zeros(Int16, nthreads())
    threaded_block(arr)
    @test arr == expected
end

test_threaded_block()

# parallel atomic addition
function threaded_atomic_add(x, n)
    @threads all for i = 1:n
        atomic_add!(x, 1)
    end
end

function test_threaded_atomic_add()
    x = Atomic()
    threaded_atomic_add(x, 10000)
    @test x[] == 10000
end

test_threaded_atomic_add()

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
    @threads all for i = m:n
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

# spin locks
function threaded_add_using_spinlock(s, x, n)
    @threads all for i = 1:n
        lock!(s)
        x = x + 1
        unlock!(s)
    end
    return x
end

function test_spinlock()
    s = SpinLock()
    x = 0
    x = threaded_add_using_spinlock(s, x, 10000)
    @test x == 10000
end

test_spinlock()

# mutexes
function threaded_add_using_mutex(m, x, n)
    @threads all for i = 1:n
        lock!(m)
        x = x + 1
        unlock!(m)
    end
    return x
end

function test_mutex()
    m = Mutex()
    x = 0
    x = threaded_add_using_mutex(m, x, 10000)
    @test x == 10000
end

test_mutex()

