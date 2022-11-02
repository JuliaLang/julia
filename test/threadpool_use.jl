# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Threads

@test nthreadpools() == 2
@test threadpool() === :default
@test threadpool(2) === :interactive
dtask() = @test threadpool(current_task()) === :default
itask() = @test threadpool(current_task()) === :interactive
dt1 = @spawn dtask()
dt2 = @spawn :default dtask()
it = @spawn :interactive itask()
wait(dt1)
wait(dt2)
wait(it)
