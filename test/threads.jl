using Base.Test
using Base.Threading

expected = [1:nthreads();]

# test 1
arr = zeros(Int16, nthreads())

function foo(A)
    tid = threadid()
    A[tid] = tid
end

@time @threads all foo(arr)

@test arr == expected


# test 2
arr = zeros(Int16, nthreads())

function bar(A)
    @threads all for i = 1:nthreads()
        tid = threadid()
        A[i] = tid
    end
end

@time bar(arr)

@test arr == expected


# test 3
arr = zeros(Int16, nthreads())

function baz(A)
    @threads all begin
        tid = threadid()
        A[tid] = tid
    end
end

@time baz(arr)

@test arr == expected

