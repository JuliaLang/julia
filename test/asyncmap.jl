# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

# Test asyncmap
@test allunique(asyncmap(x->(sleep(1.0);objectid(current_task())), 1:10))

# num tasks; note that ntasks was reinterpreted as the max allowed concurrent
# tasks instead of the number of tasks objects.
@test length(unique(asyncmap(x->(yield();objectid(current_task())), 1:20; ntasks=5))) == 20

test_ntasks(itr, n) = let concur = 0, max_concur = 0
    asyncmap(itr; ntasks=n) do _
        concur += 1
        max_concur = max(concur, max_concur)
        yield()
        concur -= 1
    end

    m = if n isa Function
        n()
    else
        (n === 0 ? 100 : n)
    end

    max_concur, m
end

(max_concur, actual) = test_ntasks(1:20, 5)
@test max_concur == actual

# default num tasks
@test length(unique(asyncmap(x->(yield();objectid(current_task())), 1:200))) == 200

(max_concur, actual) = test_ntasks(1:200, 0)
@test max_concur == actual

# ntasks as a function
let nt=0
    global nt_func
    nt_func() = (v=div(nt, 25); nt+=1; v)  # increment number of tasks by 1 for every 25th call.
                                           # nt_func() will be called initially once and then for every
                                           # iteration
end
@test length(unique(asyncmap(x->(yield();objectid(current_task())), 1:200; ntasks=nt_func))) == 200

(max_concur, actual) = test_ntasks(1:200, nt_func)
@test max_concur == actual

# batch mode tests
let ctr=0
    global next_ctr
    next_ctr() = (ctr+=1; ctr)
end
resp = asyncmap(x->(v=next_ctr(); map(_->v, x)), 1:22; ntasks=5, batch_size=5)
@test length(resp) == 22
@test length(unique(resp)) == 5

input = rand(1:1000, 100)
@test asyncmap(x->map(args->identity(args...), x), input; ntasks=5, batch_size=5) == input

# check whether shape is retained
a=rand(2,2)
b=asyncmap(identity, a)
@test a == b
@test size(a) == size(b)

# check with an iterator that does not implement size()
c=Channel(32); foreach(i->put!(c,i), 1:10); close(c)
b=asyncmap(identity, c)
@test Int[1:10...] == b
@test size(b) == (10,)

# check with an iterator that has only implements length()
len_only_iterable = (1,2,3,4,5)
@test Base.IteratorSize(len_only_iterable) == Base.HasLength()
@test asyncmap(identity, len_only_iterable) == map(identity, len_only_iterable)

# Error conditions
@test_throws ArgumentError asyncmap(identity, 1:10; batch_size=0)
@test_throws ArgumentError asyncmap(identity, 1:10; batch_size="10")
@test_throws ArgumentError asyncmap(identity, 1:10; ntasks="10")

include("generic_map_tests.jl")
generic_map_tests(asyncmap, asyncmap!)

# asyncmap with various types. Test for equivalence with map
run_map_equivalence_tests(asyncmap)
using Base.Unicode: uppercase
@test asyncmap(uppercase, "Hello World!") == map(uppercase, "Hello World!")

# Inner exceptions
@test_throws CompositeException asyncmap(_ -> error("foo"), 1:5)
@test_throws CompositeException asyncmap(_ -> error("foo"), 1:5, ntasks=2)
@test_throws CompositeException asyncmap(x -> x == 3 && error("foo"), 1:5, ntasks=2)
@test_throws CompositeException asyncmap(1:4, batch_size=2) do v
    map(u -> iseven(u) && error("foo"), v)
end

unpack(ex::CapturedException) = unpack(ex.ex)
unpack(ex::TaskFailedException) = unpack(ex.task.exception)
unpack(ex) = ex

# Make sure exceptions are thrown in the order they happen
chnl = Channel()
try
    asyncmap(1:2) do i
        i == 1 && take!(chnl)
        close(chnl)
        error("first")
    end
catch ex
    @test length(ex.exceptions) == 2
    @test unpack(ex.exceptions[1]).msg == "first"
end
