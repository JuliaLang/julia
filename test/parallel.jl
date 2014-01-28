# NOTE: worker processes cannot add more workers, only the client process can. 
require("testdefs.jl")

if nprocs() < 2
    remotecall_fetch(1, () -> addprocs(1))
end

id_me = myid()
id_other = filter(x -> x != id_me, procs())[rand(1:(nprocs()-1))]

@test fetch(@spawnat id_other myid()) == id_other
@test @fetchfrom id_other begin myid() end == id_other
@fetch begin myid() end

d = drand((200,200), [id_me, id_other])
s = convert(Array, d[1:150, 1:150])
a = convert(Array, d)
@test a[1:150,1:150] == s

@test fetch(@spawnat id_me localpart(d)[1,1]) == d[1,1]
@test fetch(@spawnat id_other localpart(d)[1,1]) == d[1,101]


if haskey(ENV, "TEST_SHARED_ARRAY")
    @unix_only begin
    # SharedArray tests
    dims = (20,20,20)
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

    d = SharedArray(Int, dims; init = D->fill!(D.loc_subarr_1d, myid()))
    for p in procs(d)
        idxes_in_p = remotecall_fetch(p, D -> parentindexes(D.loc_subarr_1d)[1], d)
        idxf = first(idxes_in_p)
        idxl = last(idxes_in_p)
        @test d[idxf] == p 
        @test d[idxl] == p 
    end

    # SharedArray as an array
    # Since the data in d will depend on the nprocs, just test that these operations work
    a = d[1:5]
    @test_throws d[-1:5]
    a = d[1,1,1:3:end]
    d[2:4] = 7
    d[5,1:2:4,8] = 19

    end # @unix_only(SharedArray tests)
end


# Test @parallel load balancing - all processors should get either M or M+1
# iterations out of the loop range for some M.
if nprocs() < 4
    remotecall_fetch(1, () -> addprocs(4 - nprocs()))
end
workloads = hist(@parallel((a,b)->[a,b], for i=1:7; myid(); end), nprocs())[2]
@test maximum(workloads) - minimum(workloads) <= 1

# @parallel reduction should work even with very short ranges
@test @parallel(+, for i=1:2; i; end) == 3

# Testing timedwait on multiple RemoteRefs
rr1 = RemoteRef()
rr2 = RemoteRef()
rr3 = RemoteRef()

@async begin sleep(0.5); put(rr1, :ok) end
@async begin sleep(1.0); put(rr2, :ok) end
@async begin sleep(2.0); put(rr3, :ok) end

tic()
timedwait(1.0) do
    all(map(isready, [rr1, rr2, rr3]))
end
et=toq()

# assuming that 0.5 seconds is a good enough buffer on a typical modern CPU
@test (et >= 1.0) && (et <= 1.5)
@test isready(rr1)
@test !isready(rr3)


# TODO: The below block should be always enabled but the error is printed by the event loop

# Hence in the event of any relevant changes to the parallel codebase,
# please define an ENV variable PTEST_FULL and ensure that the below block is 
# executed successfully before committing/merging

if haskey(ENV, "PTEST_FULL")
    println("START of parallel tests that print errors")

    # make sure exceptions propagate when waiting on Tasks
    @test_throws (@sync (@async error("oops")))

    # pmap tests
    # needs at least 4 processors (which are being created above for the @parallel tests)
    s = "a"*"bcdefghijklmnopqrstuvwxyz"^100;
    ups = "A"*"BCDEFGHIJKLMNOPQRSTUVWXYZ"^100;
    @test ups == bytestring(Uint8[uint8(c) for c in pmap(x->uppercase(x), s)])
    @test ups == bytestring(Uint8[uint8(c) for c in pmap(x->uppercase(char(x)), s.data)])

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
    @test ups == bytestring(Uint8[uint8(c) for c in res])

    # no retry, on error continue
    res = pmap(x->(x=='a') ? error("test error. don't panic.") : uppercase(x), s; err_retry=false, err_stop=false);
    @test length(res) == length(ups)
    @test isa(res[1], Exception)
    
    println("END of parallel tests that print errors")
end

