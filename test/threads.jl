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

