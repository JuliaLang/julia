# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Threads

@test nthreadpools() == 2
@test threadpool() === :interactive
@test threadpool(2) === :default
@test fetch(Threads.@spawn Threads.threadpool()) === :default
@test fetch(Threads.@spawn :default Threads.threadpool()) === :default
@test fetch(Threads.@spawn :interactive Threads.threadpool()) === :interactive
@test fetch(Threads.@spawn :samepool Threads.threadpool()) === Threads.threadpool()
@sync for tp in [:interactive, :default]
    Threads.@spawn tp begin
        @test fetch(Threads.@spawn :samepool Threads.threadpool()) === Threads.threadpool()
    end
end
wait(Threads.@spawn :interactive begin
    @test fetch(Threads.@spawn :samepool Threads.threadpool()) === Threads.threadpool()
end)
tp = :default
@test fetch(Threads.@spawn tp Threads.threadpool()) === :default
tp = :interactive
@test fetch(Threads.@spawn tp Threads.threadpool()) === :interactive
tp = :foo
@test_throws ArgumentError Threads.@spawn tp Threads.threadpool()
@test Threads.threadpooltids(:interactive) == [1]
@test Threads.threadpooltids(:default) == [2]
