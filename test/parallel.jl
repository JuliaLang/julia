# NOTE: worker processes cannot add more workers, only the client process can.
require("testdefs.jl")

if nprocs() < 3
    remotecall_fetch(1, () -> addprocs(2))
end

id_me = myid()
id_other = filter(x -> x != id_me, procs())[rand(1:(nprocs()-1))]

@test fetch(@spawnat id_other myid()) == id_other
@test @fetchfrom id_other begin myid() end == id_other
@fetch begin myid() end

d = drand((200,200), [id_me, id_other])
s = convert(Matrix{Float64}, d[1:150, 1:150])
a = convert(Matrix{Float64}, d)
@test a[1:150,1:150] == s

@test fetch(@spawnat id_me localpart(d)[1,1]) == d[1,1]
@test fetch(@spawnat id_other localpart(d)[1,1]) == d[1,101]

d=DArray(I->fill(myid(), map(length,I)), (10,10), [id_me, id_other])
d2 = map(x->1, d)
@test reduce(+, d2) == 100

@test reduce(+, d) == ((50*id_me) + (50*id_other))
map!(x->1, d)
@test reduce(+, d) == 100


dims = (20,20,20)

@linux_only begin
    S = SharedArray(Int64, dims)
    @test beginswith(S.segname, "/jl")
    @test !ispath("/dev/shm" * S.segname)

    S = SharedArray(Int64, dims; pids=[id_other])
    @test beginswith(S.segname, "/jl")
    @test !ispath("/dev/shm" * S.segname)
end

# TODO : Need a similar test of shmem cleanup for OSX

# SharedArray tests
d = Base.shmem_rand(1:100, dims)
a = convert(Array, d)

partsums = Array(Int, length(procs(d)))
@sync begin
    for (i, p) in enumerate(procs(d))
        @async partsums[i] = remotecall_fetch(p, D->sum(D.loc_subarr_1d), d)
    end
end
@test sum(a) == sum(partsums)

d = Base.shmem_rand(dims)
for p in procs(d)
    idxes_in_p = remotecall_fetch(p, D -> parentindexes(D.loc_subarr_1d)[1], d)
    idxf = first(idxes_in_p)
    idxl = last(idxes_in_p)
    d[idxf] = float64(idxf)
    rv = remotecall_fetch(p, (D,idxf,idxl) -> begin assert(D[idxf] == float64(idxf)); D[idxl] = float64(idxl); D[idxl];  end, d,idxf,idxl)
    @test d[idxl] == rv
end

@test ones(10, 10, 10) == Base.shmem_fill(1.0, (10,10,10))
@test zeros(Int32, 10, 10, 10) == Base.shmem_fill(0, (10,10,10))

d = Base.shmem_rand(dims)
s = Base.shmem_rand(dims)
copy!(s, d)
@test s == d
s = Base.shmem_rand(dims)
copy!(s, sdata(d))
@test s == d

d = SharedArray(Int, dims; init = D->fill!(D.loc_subarr_1d, myid()))
for p in procs(d)
    idxes_in_p = remotecall_fetch(p, D -> parentindexes(D.loc_subarr_1d)[1], d)
    idxf = first(idxes_in_p)
    idxl = last(idxes_in_p)
    @test d[idxf] == p
    @test d[idxl] == p
end

# issue #6362
d = Base.shmem_rand(dims)
s = copy(sdata(d))
ds = deepcopy(d)
@test ds == d
pids_ds = procs(ds)
remotecall_fetch(pids_ds[findfirst(id->(id != myid()), pids_ds)], setindex!, ds, 1.0, 1:10)
@test ds != d
@test s == d


# SharedArray as an array
# Since the data in d will depend on the nprocs, just test that these operations work
a = d[1:5]
@test_throws BoundsError d[-1:5]
a = d[1,1,1:3:end]
d[2:4] = 7
d[5,1:2:4,8] = 19

AA = rand(4,2)
A = convert(SharedArray, AA)
B = convert(SharedArray, AA')
@test B*A == AA'*AA

d=SharedArray(Int64, (10,10); init = D->fill!(D.loc_subarr_1d, myid()), pids=[id_me, id_other])
d2 = map(x->1, d)
@test reduce(+, d2) == 100

@test reduce(+, d) == ((50*id_me) + (50*id_other))
map!(x->1, d)
@test reduce(+, d) == 100

# Boundary cases where length(S) <= length(pids)
@test 2.0 == remotecall_fetch(id_other, D->D[2], Base.shmem_fill(2.0, 2; pids=[id_me, id_other]))
@test 3.0 == remotecall_fetch(id_other, D->D[1], Base.shmem_fill(3.0, 1; pids=[id_me, id_other]))


# Test @parallel load balancing - all processors should get either M or M+1
# iterations out of the loop range for some M.
if nprocs() < 4
    remotecall_fetch(1, () -> addprocs(4 - nprocs()))
end
workloads = hist(@parallel((a,b)->[a,b], for i=1:7; myid(); end), nprocs())[2]
@test maximum(workloads) - minimum(workloads) <= 1

# @parallel reduction should work even with very short ranges
@test @parallel(+, for i=1:2; i; end) == 3

# Testing timedwait on multiple RemoteChannels
rr1 = RemoteChannel()
rr2 = RemoteChannel()
rr3 = RemoteChannel()

@async begin sleep(0.5); put!(rr1, :ok) end
@async begin sleep(1.0); put!(rr2, :ok) end
@async begin sleep(2.0); put!(rr3, :ok) end

tic()
timedwait(1.0) do
    all(map(isready, [rr1, rr2, rr3]))
end
et=toq()

# assuming that 0.5 seconds is a good enough buffer on a typical modern CPU
try
    @test (et >= 1.0) && (et <= 1.5)
    @test !isready(rr3)
catch
    warn("timedwait tests delayed. et=$et, isready(rr3)=$(isready(rr3))")
end
@test isready(rr1)

function test_channel(c)
    put!(c, 1)
    put!(c, "Hello")
    put!(c, 5.0)

    @test isready(c) == true
    @test fetch(c) == 1
    @test fetch(c) == 1   # Should not have been popped previously
    @test take!(c) == 1
    @test take!(c) == "Hello"
    @test fetch(c) == 5.0
    @test take!(c) == 5.0
    @test isready(c) == false
end

test_channel(RemoteChannel(; sz=10))

# same test mixed with another worker...
c = RemoteChannel(id_other; sz=10)
put!(c, 1)
remotecall_fetch(id_other, ch -> put!(ch, "Hello"), c)
put!(c, 5.0)

@test isready(c) == true
@test remotecall_fetch(id_other, ch -> fetch(ch), c) == 1
@test fetch(c) == 1   # Should not have been popped previously
@test take!(c) == 1
@test remotecall_fetch(id_other, ch -> take!(ch), c) == "Hello"
@test fetch(c) == 5.0
@test remotecall_fetch(id_other, ch -> take!(ch), c) == 5.0
@test remotecall_fetch(id_other, ch -> isready(ch), c) == false
@test isready(c) == false


test_channel(Channel(10))

c=Channel(Int)
@test_throws MethodError put!(c, "Hello")

c=Channel(256)
@test c.szp1 <= 33
for x in 1:40
  put!(c, x)
end
@test c.szp1 <= 65
for x in 1:39
  take!(c)
end
for x in 1:64
  put!(c, x)
end
@test (c.szp1 > 65) && (c.szp1 <= 129)
for x in 1:39
  take!(c)
end
@test fetch(c) == 39
for x in 1:26
  take!(c)
end
@test isready(c) == false

n=4
c=Channel(n)
for i in 1:n
    @test isready(c) == false
    for j in 1:i
        put!(c, j)
    end
    @test isready(c) == true
    for j in 1:i
        @test take!(c) == j
    end
    @test isready(c) == false
end

# TODO: The below block should be always enabled but the error is printed by the event loop

# Hence in the event of any relevant changes to the parallel codebase,
# please define an ENV variable PTEST_FULL and ensure that the below block is
# executed successfully before committing/merging

if haskey(ENV, "PTEST_FULL")
    println("START of parallel tests that print errors")

    # make sure exceptions propagate when waiting on Tasks
    @test_throws ErrorException (@sync (@async error("oops")))

    # pmap tests
    # needs at least 4 processors (which are being created above for the @parallel tests)
    s = "a"*"bcdefghijklmnopqrstuvwxyz"^100;
    ups = "A"*"BCDEFGHIJKLMNOPQRSTUVWXYZ"^100;
    @test ups == bytestring(UInt8[uint8(c) for c in pmap(x->uppercase(x), s)])
    @test ups == bytestring(UInt8[uint8(c) for c in pmap(x->uppercase(char(x)), s.data)])

    # retry, on error exit
    res = pmap(x->(x=='a') ? error("test error. don't panic.") : uppercase(x), s; err_retry=true, err_stop=true);
    @test length(res) < length(ups)
    @test isa(res[1], Exception)

    # no retry, on error exit
    res = pmap(x->(x=='a') ? error("test error. don't panic.") : uppercase(x), s; err_retry=false, err_stop=true);
    @test length(res) < length(ups)
    @test isa(res[1], Exception)

    # retry, on error continue
    res = pmap(x->iseven(myid()) ? error("test error. don't panic.") : uppercase(x), s; err_retry=true, err_stop=false);
    @test length(res) == length(ups)
    @test ups == bytestring(UInt8[uint8(c) for c in res])

    # no retry, on error continue
    res = pmap(x->(x=='a') ? error("test error. don't panic.") : uppercase(x), s; err_retry=false, err_stop=false);
    @test length(res) == length(ups)
    @test isa(res[1], Exception)

    println("END of parallel tests that print errors")
end

# issue #7727
let A = [], B = []
    t = @task produce(11)
    @sync begin
        @async for x in t; push!(A,x); end
        @async for x in t; push!(B,x); end
    end
    @test (A == [11]) != (B == [11])
end
