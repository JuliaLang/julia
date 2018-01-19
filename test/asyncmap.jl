# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

# Test asyncmap
@test allunique(asyncmap(x->(sleep(1.0);objectid(current_task())), 1:10))

# num tasks
@test length(unique(asyncmap(x->(yield();objectid(current_task())), 1:20; ntasks=5))) == 5

# default num tasks
@test length(unique(asyncmap(x->(yield();objectid(current_task())), 1:200))) == 100

# ntasks as a function
let nt=0
    global nt_func
    nt_func() = (v=div(nt, 25); nt+=1; v)  # increment number of tasks by 1 for every 25th call.
                                           # nt_func() will be called initally once and then for every
                                           # iteration
end
@test length(unique(asyncmap(x->(yield();objectid(current_task())), 1:200; ntasks=nt_func))) == 7

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
