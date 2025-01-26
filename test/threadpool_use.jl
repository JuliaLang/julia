# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Threads

@test nthreadpools() == 2
@test threadpool() === :interactive
@test threadpool(2) === :default
@test fetch(Threads.@spawn Threads.threadpool()) === :default
@test fetch(Threads.@spawn :default Threads.threadpool()) === :default
@test fetch(Threads.@spawn :interactive Threads.threadpool()) === :interactive
tp = :default
@test fetch(Threads.@spawn tp Threads.threadpool()) === :default
tp = :interactive
@test fetch(Threads.@spawn tp Threads.threadpool()) === :interactive
tp = :foo
@test_throws ArgumentError Threads.@spawn tp Threads.threadpool()
@test Threads.threadpooltids(:interactive) == [1]
@test Threads.threadpooltids(:default) == [2]

# Test with custom stack size:
@test fetch(Threads.@spawn reserved_stack=0 Threads.threadpool()) === :default

# With both threadpool and stack size
@test fetch(Threads.@spawn :default reserved_stack=0 Threads.threadpool()) === :default

# Order should not matter:
@test fetch(Threads.@spawn reserved_stack=0 :default Threads.threadpool()) === :default

# Can set an expression for reserved_stack
tp = :interactive
@test fetch(Threads.@spawn reserved_stack=(4 * 1024^2) tp Threads.threadpool()) === :interactive

# Can also pass variables:
reserved_stack = 4 * 1024^2
@test fetch(Threads.@spawn reserved_stack=reserved_stack :interactive Threads.threadpool()) === :interactive
