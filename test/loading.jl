# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

expect_load_from = 1
include("test_sourcepath.jl") # defines "path", consumes "expect_load_from"

thefname = "the fname!//\\&\0\1*"
@test include_string("@__FILE__", thefname) == ":string:\0$thefname"
@test include_string("@__FILE__") == ":string:\0none"
@test basename(@__FILE__) == "loading.jl"
if myid() != 1
@test startswith(@__FILE__, ":node 1:\0")
else
@test isabspath(@__FILE__)
end

# the following may fail if this worker is not on the same machine as 1
# since it tries to include the file off the local worker
expect_load_from = myid()
include("test_sourcepath.jl", (expect_load_from, dirname(path[2])))
if nprocs() > 1
    expect_load_from = filter(x -> x != myid(), procs())[rand(1:(nprocs()-1))]
    include("test_sourcepath.jl", (expect_load_from, dirname(path[2])))
    @spawnat 1 begin
        eval(Main, :(expect_load_from = 1))
        include("test_sourcepath.jl") # defines "path", consumes "expect_load_from"
    end
end
