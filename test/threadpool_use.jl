# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Threads

@test nthreadpools() == 2
@test threadpool() == THREADPOOL_DEFAULT
@test threadpool(2) == THREADPOOL_INTERACTIVE
@test nthreads(THREADPOOL_DEFAULT) == nthreads(:default)
@test nthreads(THREADPOOL_INTERACTIVE) == nthreads(:interactive)
@test nthreads(THREADPOOL_DEFAULT) + nthreads(THREADPOOL_INTERACTIVE) == nthreads()
dtask() = @test threadpool(current_task()) == THREADPOOL_DEFAULT
itask() = @test threadpool(current_task()) == THREADPOOL_INTERACTIVE
dt1 = @spawn dtask()
dt2 = @spawn :default dtask()
it = @spawn :interactive itask()
wait(dt1)
wait(dt2)
wait(it)
