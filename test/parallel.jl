# NOTE: worker processes cannot add more workers, only the client process can.
require("testdefs.jl")

if nworkers() < 3
    remotecall_fetch(1, () -> addprocs(3))
end

id_me = myid()
id_other = filter(x -> x != id_me, procs())[rand(1:(nprocs()-1))]

@test fetch(@spawnat id_other myid()) == id_other
@test @fetchfrom id_other begin myid() end == id_other
@fetch begin myid() end

d = drand((200,200), [id_me, id_other])
dc = copy(d)

@test d == dc                                           # Should be identical
@spawnat id_other localpart(dc)[1] = 0
@test fetch(@spawnat id_other localpart(d)[1] != 0) # but not point to the same memory

s = convert(Matrix{Float64}, d[1:150, 1:150])
a = convert(Matrix{Float64}, d)
@test a[1:150,1:150] == s

@test fetch(@spawnat id_me localpart(d)[1,1]) == d[1,1]
@test fetch(@spawnat id_other localpart(d)[1,1]) == d[1,101]

d = DArray(I->fill(myid(), map(length,I)), (10,10), [id_me, id_other])
d2 = map(x->1, d)
@test reduce(+, d2) == 100

@test reduce(+, d) == ((50*id_me) + (50*id_other))
map!(x->1, d)
@test reduce(+, d) == 100

# Test mapreduce on DArrays
begin
    # Test that the proper method exists on DArrays
    sig = methods(mapreduce, (Function, Function, DArray))[1].sig
    @test sig[3] == DArray

    # Test that it is functionally equivalent to the standard method
    for _ = 1:25, f = [x -> 2x, x -> x^2, x -> x^2 + 2x - 1], opt = [+, *]
        n = rand(5:50)
        arr = rand(1:100, n)
        darr = distribute(arr)

        @test mapreduce(f, opt, arr) == mapreduce(f, opt, darr)
    end
end

# Test mapreducedim on DArrays
@test mapreducedim(t -> t*t, +, d2, 1) == mapreducedim(t -> t*t, +, convert(Array, d2), 1)
@test mapreducedim(t -> t*t, +, d2, 2) == mapreducedim(t -> t*t, +, convert(Array, d2), 2)
@test mapreducedim(t -> t*t, +, d2, (1,2)) == mapreducedim(t -> t*t, +, convert(Array, d2), (1,2))

dims = (20,20,20)

d = drandn(dims)
da = convert(Array, d)
for dms in (1, 2, 3, (1,2), (1,3), (2,3), (1,2,3))
    @test_approx_eq mapreducedim(t -> t*t, +, d, dms) mapreducedim(t -> t*t, +, da, dms)
    @test_approx_eq mapreducedim(t -> t*t, +, d, dms, 1.0) mapreducedim(t -> t*t, +, da, dms, 1.0)

    @test_approx_eq reducedim(*, d, dms) reducedim(*, da, dms)
    @test_approx_eq reducedim(*, d, dms, 2.0) reducedim(*, da, dms, 2.0)

    # statistical function (works generically through calls to mapreducedim!, i.e. not implemented specifically for DArrays)
    @test_approx_eq mean(d, dms) mean(da, dms)
    # @test_approx_eq std(d, dms) std(da, dms) Requires centralize_sumabs2! for DArrays
end

@linux_only begin
    S = SharedArray(Int64, dims)
    @test startswith(S.segname, "/jl")
    @test !ispath("/dev/shm" * S.segname)

    S = SharedArray(Int64, dims; pids=[id_other])
    @test startswith(S.segname, "/jl")
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
@test B*A == ctranspose(AA)*AA

d=SharedArray(Int64, (10,10); init = D->fill!(D.loc_subarr_1d, myid()), pids=[id_me, id_other])
d2 = map(x->1, d)
@test reduce(+, d2) == 100

@test reduce(+, d) == ((50*id_me) + (50*id_other))
map!(x->1, d)
@test reduce(+, d) == 100

@test fill!(d, 1) == ones(10, 10)
@test fill!(d, 2.) == fill(2, 10, 10)

# Boundary cases where length(S) <= length(pids)
@test 2.0 == remotecall_fetch(id_other, D->D[2], Base.shmem_fill(2.0, 2; pids=[id_me, id_other]))
@test 3.0 == remotecall_fetch(id_other, D->D[1], Base.shmem_fill(3.0, 1; pids=[id_me, id_other]))


# Test @parallel load balancing - all processors should get either M or M+1
# iterations out of the loop range for some M.
workloads = hist(@parallel((a,b)->[a;b], for i=1:7; myid(); end), nprocs())[2]
@test maximum(workloads) - minimum(workloads) <= 1

# @parallel reduction should work even with very short ranges
@test @parallel(+, for i=1:2; i; end) == 3

# Testing timedwait on multiple RemoteRefs
@sync begin
    rr1 = RemoteRef()
    rr2 = RemoteRef()
    rr3 = RemoteRef()

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
end

# specify pids for pmap
@test sort(workers()[1:2]) == sort(unique(pmap(x->(sleep(0.1);myid()), 1:10, pids = workers()[1:2])))

# Testing buffered  and unbuffered reads
# This large array should write directly to the socket
a = ones(10^6)
@test a == remotecall_fetch(id_other, (x)->x, a)

# Not a bitstype, should be buffered
s = [randstring() for x in 1:10^5]
@test s == remotecall_fetch(id_other, (x)->x, s)

#large number of small requests
num_small_requests = 10000
@test fill(id_other, num_small_requests) == [remotecall_fetch(id_other, myid) for i in 1:num_small_requests]

# test parallel sends of large arrays from multiple tasks to the same remote worker
ntasks = 10
rr_list = [RemoteRef() for x in 1:ntasks]
for rr in rr_list
    @async let rr=rr
        a=ones(10^6);
        try
            for i in 1:10
                @test a == remotecall_fetch(id_other, (x)->x, a)
                yield()
            end
            put!(rr, :OK)
        catch
            put!(rr, :ERROR)
        end
    end
end

@test [fetch(rr) for rr in rr_list] == [:OK for x in 1:ntasks]

# TODO: The below block should be always enabled but the error is printed by the event loop

# Hence in the event of any relevant changes to the parallel codebase,
# please define an ENV variable PTEST_FULL and ensure that the below block is
# executed successfully before committing/merging

if haskey(ENV, "PTEST_FULL")
    print("\n\nSTART of parallel tests that print errors\n")

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

    print("\n\nEND of parallel tests that print errors\n")

@unix_only begin
    #Issue #9951
    hosts=[]
    for i in 1:30
        push!(hosts, "localhost", string(getipaddr()), "127.0.0.1")
    end

    print("\nTesting SSH addprocs with $(length(hosts)) workers...\n")
    new_pids = remotecall_fetch(1, addprocs, hosts)
#    print("Added workers $new_pids\n\n")
    function test_n_remove_pids(new_pids)
        for p in new_pids
            w_in_remote = sort(remotecall_fetch(p, workers))
            try
                @test intersect(new_pids, w_in_remote) == new_pids
            catch e
                print("p       :     $p\n")
                print("newpids :     $new_pids\n")
                print("intersect   : $(intersect(new_pids, w_in_remote))\n\n\n")
                rethrow(e)
            end
        end

        @test :ok == remotecall_fetch(1, (p)->rmprocs(p; waitfor=5.0), new_pids)
    end

    test_n_remove_pids(new_pids)

    print("\nMixed ssh addprocs with :auto\n")
    new_pids = sort(remotecall_fetch(1, addprocs, ["localhost", ("127.0.0.1", :auto), "localhost"]))
    @test length(new_pids) == (2 + Sys.CPU_CORES)
    test_n_remove_pids(new_pids)

    print("\nMixed ssh addprocs with numbers\n")
    new_pids = sort(remotecall_fetch(1, addprocs, [("localhost", 2), ("127.0.0.1", 2), "localhost"]))
    @test length(new_pids) == 5
    test_n_remove_pids(new_pids)

    print("\nssh addprocs with tunnel\n")
    new_pids = sort(remotecall_fetch(1, ()->addprocs([("localhost", 2)]; tunnel=true)))
    @test length(new_pids) == 2
    test_n_remove_pids(new_pids)

end
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
