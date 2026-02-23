# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Threads

c = Channel{Symbol}() do c; put!(c, threadpool(current_task())); end
@test take!(c) === threadpool(current_task())
c = Channel{Symbol}(spawn = true) do c; put!(c, threadpool(current_task())); end
@test take!(c) === :default
c = Channel{Symbol}(threadpool = :interactive) do c; put!(c, threadpool(current_task())); end
@test take!(c) === :interactive
@test_throws ArgumentError Channel{Symbol}(threadpool = :foo) do c; put!(c, :foo); end
