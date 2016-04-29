# This file is a part of Julia. License is MIT: http://julialang.org/license

dir = joinpath(JULIA_HOME, Base.DOCDIR, "examples")

include(joinpath(dir, "bubblesort.jl"))
a = rand(1:100,100)
@test issorted(sort!(a;alg=BubbleSort))

include(joinpath(dir, "lru.jl"))
include(joinpath(dir, "lru_test.jl"))

include(joinpath(dir, "modint.jl"))
b = ModInts.ModInt{10}(2)
c = ModInts.ModInt{10}(4)
@test b + c == ModInts.ModInt{10}(6)
@test c - b == ModInts.ModInt{10}(2)
x = ModInts.ModInt{256}(13)
y = inv(x)
@test y == ModInts.ModInt{256}(197)
@test x*y == ModInts.ModInt{256}(1)
@test_throws ErrorException inv(ModInts.ModInt{8}(4))

include(joinpath(dir, "ndgrid.jl"))
r = repmat(1:10,1,10)
r1, r2 = ndgrid(1:10, 1:10)
@test r1 == r
@test r2 == r'
r3, r4 = meshgrid(1:10,1:10)
@test r3 == r'
@test r4 == r

include(joinpath(dir, "queens.jl"))
@test solve(8, 8, 1) == Array{Int,1}[[1,1]]
@test solve(8, 8, 7) == Array{Int,1}[[1,1],[2,3],[3,5],[4,2],[5,8],[7,4],[8,7]]

# Different cluster managers do not play well together. Since
# the test infrastructure already uses LocalManager, we will test the simple
# cluster manager example through a new Julia session.
@unix_only begin
    script = joinpath(dir, "clustermanager/simple/test_simple.jl")
    cmd = `$(Base.julia_cmd()) $script`
    if !success(pipeline(cmd; stdout=STDOUT, stderr=STDERR)) && ccall(:jl_running_on_valgrind,Cint,()) == 0
        error("UnixDomainCM failed test, cmd : $cmd")
    end
end

dc_path = joinpath(dir, "dictchannel.jl")
myid() == 1 || include(dc_path)

# The checks to see if DictChannel is defined is to identify the root cause of
# https://github.com/JuliaLang/julia/issues/16091 .
# To be removed once fixed.
defined_on_worker = false
if (myid() != 1) && !isdefined(:DictChannel)
    error("myid : $(myid()). DictChannel not defined.")
else
    defined_on_worker = true
end

# Run the remote on pid 1, since runtests may terminate workers
# at any time depending on memory usage
@test remotecall_fetch(1, dc_path) do f
    include(f)
    return :infungible
end === :infungible

defined_on_master = false
if remotecall_fetch(isdefined, 1, :DictChannel) == false
    error("myid : $(myid()). DictChannel not defined on 1")
else
    defined_on_master = true
end

dc = nothing
try
    dc=RemoteChannel(()->DictChannel(), 1)
catch e
    println("myid : ", myid(), ", dc_path : ", dc_path, ", defined_on_worker:", defined_on_worker, ", defined_on_master: ", defined_on_master)
    rethrow(e)
end
@test typeof(dc) == RemoteChannel{DictChannel}

@test isready(dc) == false
put!(dc, 1, 2)
put!(dc, "Hello", "World")
@test isready(dc) == true
@test isready(dc, 1) == true
@test isready(dc, "Hello") == true
@test isready(dc, 2) == false
@test fetch(dc, 1) == 2
@test fetch(dc, "Hello") == "World"
@test take!(dc, 1) == 2
@test isready(dc, 1) == false


# At least make sure code loads
include(joinpath(dir, "wordcount.jl"))

# the 0mq clustermanager depends on package ZMQ. Just making sure the
# code loads using a stub module definition for ZMQ.
zmq_found=true
try
    using ZMQ
catch
    zmq_found=false
end

if !zmq_found
    eval(parse("module ZMQ end"))
end

include(joinpath(dir, "clustermanager/0mq/ZMQCM.jl"))

