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
@test_throws DomainError inv(ModInts.ModInt{8}(4))

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
if is_unix()
    script = joinpath(dir, "clustermanager/simple/test_simple.jl")
    cmd = `$(Base.julia_cmd()) --startup-file=no $script`
    if !success(pipeline(cmd; stdout=STDOUT, stderr=STDERR)) && ccall(:jl_running_on_valgrind,Cint,()) == 0
        error("UnixDomainCM failed test, cmd : $cmd")
    end
end

dc_path = joinpath(dir, "dictchannel.jl")
# Run the remote on pid 1, since runtests may terminate workers
# at any time depending on memory usage
main_ex = quote
    myid() == 1 || include($dc_path)
    remotecall_fetch(1, $dc_path) do f
        include(f)
        nothing
    end
    RemoteChannel(()->DictChannel(), 1)
end
dc = eval(Main, main_ex)
@test typeof(dc) == RemoteChannel{Main.DictChannel}

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
    eval(Main, parse("module ZMQ end"))
end

include(joinpath(dir, "clustermanager/0mq/ZMQCM.jl"))
