# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Threads

# This test has not been added to CI as there can be unexpected delays
# which cause timing-dependent actions to fail.

#=
Test to ensure that the interactive threadpool works as designed.

Task A is a standard task that does a lot of work (~2 seconds) without
yielding. This would prevent ordinarily prevent other tasks from running.

Task B is an interactive task that does a little work (~0.02 seconds) and
yields.

With an interactive threadpool, multiple task Bs should not see notable
delays in execution even when multiple task As are occupying Julia's
default threads.

This test should fail in the absence of an interactive thread.
=#
const N = 263000000 # busywork(N) takes ~1 sec on an i7-9750H @ 2.6GHz
function busywork(n::Int)
    acc = 0
    for i = 1:n
        x = rand(2:10)
        acc += i * x
    end
    return acc
end

function itask()
    h = N ÷ 50
    for i = 1:100
        t1 = time()
        busywork(h)
        yield()
        t2 = time()
        @test t2 - t1 < 0.15
    end
end

it1 = @spawn :interactive itask()
ti1 = @spawn busywork(N * 2);
it2 = @spawn :interactive itask()
ti2 = @spawn busywork(N * 2);
wait(it1)
wait(it2)
