# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
include("testenv.jl")

# Test a few "remote" invocations when no workers are present
@test remote(myid)() == 1
@test pmap(identity, 1:100) == [1:100...]
@test 100 == @parallel (+) for i in 1:100
        1
    end

addprocs_with_testenv(4)
@test nprocs() == 5

@everywhere using Test

id_me = myid()
id_other = filter(x -> x != id_me, procs())[rand(1:(nprocs()-1))]

# Test remote()
let
    pool = Base.default_worker_pool()

    count = 0
    count_condition = Condition()

    function remote_wait(c)
        @async begin
            count += 1
            remote(take!)(c)
            count -= 1
            notify(count_condition)
        end
        yield()
    end

    testchannels = [RemoteChannel() for i in 1:nworkers()]
    testcount = 0
    @test isready(pool) == true
    for c in testchannels
        @test count == testcount
        remote_wait(c)
        testcount += 1
    end
    @test count == testcount
    @test isready(pool) == false

    for c in testchannels
        @test count == testcount
        put!(c, "foo")
        testcount -= 1
        wait(count_condition)
        @test count == testcount
        @test isready(pool) == true
    end

    @test count == 0

    for c in testchannels
        @test count == testcount
        remote_wait(c)
        testcount += 1
    end
    @test count == testcount
    @test isready(pool) == false

    for c in reverse(testchannels)
        @test count == testcount
        put!(c, "foo")
        testcount -= 1
        wait(count_condition)
        @test count == testcount
        @test isready(pool) == true
    end

    @test count == 0
end

# Test Futures
function testf(id)
    f=Future(id)
    @test isready(f) == false
    @test isnull(f.v) == true
    put!(f, :OK)
    @test isready(f) == true
    @test isnull(f.v) == false

    @test_throws ErrorException put!(f, :OK) # Cannot put! to a already set future
    @test_throws MethodError take!(f) # take! is unsupported on a Future

    @test fetch(f) == :OK
end

testf(id_me)
testf(id_other)

# Distributed GC tests for Futures
function test_futures_dgc(id)
    f = remotecall(myid, id)
    fid = Base.remoteref_id(f)

    # remote value should be deleted after a fetch
    @test remotecall_fetch(k->(yield();haskey(Base.Distributed.PGRP.refs, k)), id, fid) == true
    @test isnull(f.v) == true
    @test fetch(f) == id
    @test isnull(f.v) == false
    yield(); # flush gc msgs
    @test remotecall_fetch(k->(yield();haskey(Base.Distributed.PGRP.refs, k)), id, fid) == false


    # if unfetched, it should be deleted after a finalize
    f = remotecall(myid, id)
    fid = Base.remoteref_id(f)
    @test remotecall_fetch(k->(yield();haskey(Base.Distributed.PGRP.refs, k)), id, fid) == true
    @test isnull(f.v) == true
    finalize(f)
    yield(); # flush gc msgs
    @test remotecall_fetch(k->(yield();haskey(Base.Distributed.PGRP.refs, k)), id, fid) == false
end

test_futures_dgc(id_me)
test_futures_dgc(id_other)

# if sent to another worker, it should not be deleted till all references are fetched.
wid1 = workers()[1]
wid2 = workers()[2]
f = remotecall(myid, wid1)
fid = Base.remoteref_id(f)

fstore = RemoteChannel(wid2)
put!(fstore, f)

@test fetch(f) == wid1
@test remotecall_fetch(k->haskey(Base.Distributed.PGRP.refs, k), wid1, fid) == true
remotecall_fetch(r->(fetch(fetch(r)); yield()), wid2, fstore)
sleep(0.5) # to ensure that wid2 gc messages have been executed on wid1
@test remotecall_fetch(k->haskey(Base.Distributed.PGRP.refs, k), wid1, fid) == false

# put! should release remote reference since it would have been cached locally
f = Future(wid1)
fid = Base.remoteref_id(f)

# should not be created remotely till accessed
@test remotecall_fetch(k->haskey(Base.Distributed.PGRP.refs, k), wid1, fid) == false
# create it remotely
isready(f)

@test remotecall_fetch(k->haskey(Base.Distributed.PGRP.refs, k), wid1, fid) == true
put!(f, :OK)
@test remotecall_fetch(k->haskey(Base.Distributed.PGRP.refs, k), wid1, fid) == false
@test fetch(f) == :OK

# RemoteException should be thrown on a put! when another process has set the value
f = Future(wid1)
fid = Base.remoteref_id(f)

fstore = RemoteChannel(wid2)
put!(fstore, f) # send f to wid2
put!(f, :OK) # set value from master

@test remotecall_fetch(k->haskey(Base.Distributed.PGRP.refs, k), wid1, fid) == true

testval = remotecall_fetch(wid2, fstore) do x
    try
        put!(fetch(x), :OK)
        return 0
    catch e
        if isa(e, RemoteException)
            return 1
        else
            return 2
        end
    end
end
@test testval == 1

# Distributed GC tests for RemoteChannels
function test_remoteref_dgc(id)
    rr = RemoteChannel(id)
    put!(rr, :OK)
    rrid = Base.remoteref_id(rr)

    # remote value should be deleted after finalizing the ref
    @test remotecall_fetch(k->(yield();haskey(Base.Distributed.PGRP.refs, k)), id, rrid) == true
    @test fetch(rr) == :OK
    @test remotecall_fetch(k->(yield();haskey(Base.Distributed.PGRP.refs, k)), id, rrid) == true
    finalize(rr)
    yield(); # flush gc msgs
    @test remotecall_fetch(k->(yield();haskey(Base.Distributed.PGRP.refs, k)), id, rrid) == false
end
test_remoteref_dgc(id_me)
test_remoteref_dgc(id_other)

# if sent to another worker, it should not be deleted till the other worker has also finalized.
let wid1 = workers()[1],
    wid2 = workers()[2],
    rr = RemoteChannel(wid1),
    rrid = Base.remoteref_id(rr),
    fstore = RemoteChannel(wid2)

    put!(fstore, rr)
    @test remotecall_fetch(k -> haskey(Base.Distributed.PGRP.refs, k), wid1, rrid) == true
    finalize(rr) # finalize locally
    yield() # flush gc msgs
    @test remotecall_fetch(k -> haskey(Base.Distributed.PGRP.refs, k), wid1, rrid) == true
    remotecall_fetch(r -> (finalize(take!(r)); yield(); nothing), wid2, fstore) # finalize remotely
    sleep(0.5) # to ensure that wid2 messages have been executed on wid1
    @test remotecall_fetch(k -> haskey(Base.Distributed.PGRP.refs, k), wid1, rrid) == false
end

# Tests for issue #23109 - should not hang.
f = @spawn rand(1, 1)
@sync begin
    for _ in 1:10
        @async fetch(f)
    end
end

wid1, wid2 = workers()[1:2]
f = @spawnat wid1 rand(1,1)
@sync begin
    @async fetch(f)
    @async remotecall_fetch(()->fetch(f), wid2)
end


@test fetch(@spawnat id_other myid()) == id_other
@test (@fetchfrom id_other myid()) == id_other

pids=[]
for i in 1:nworkers()
    push!(pids, @fetch myid())
end
@test sort(pids) == sort(workers())


# test getindex on Futures and RemoteChannels
function test_indexing(rr)
    a = rand(5,5)
    put!(rr, a)
    @test rr[2,3] == a[2,3]
    @test rr[] == a
end

test_indexing(Future())
test_indexing(Future(id_other))
test_indexing(RemoteChannel())
test_indexing(RemoteChannel(id_other))

# Test ser/deser to non-ClusterSerializer objects.
function test_regular_io_ser(ref::Base.Distributed.AbstractRemoteRef)
    io = IOBuffer()
    serialize(io, ref)
    seekstart(io)
    ref2 = deserialize(io)
    for fld in fieldnames(typeof(ref))
        v = getfield(ref2, fld)
        if isa(v, Number)
            @test v === zero(typeof(v))
        elseif isa(v, Nullable)
            @test v === Nullable{Any}()
        else
            error(string("Add test for field ", fld))
        end
    end
end

test_regular_io_ser(Future())
test_regular_io_ser(RemoteChannel())

dims = (20,20,20)

if Sys.islinux()
    S = SharedArray{Int64,3}(dims)
    @test startswith(S.segname, "/jl")
    @test !ispath("/dev/shm" * S.segname)

    S = SharedArray{Int64,3}(dims; pids=[id_other])
    @test startswith(S.segname, "/jl")
    @test !ispath("/dev/shm" * S.segname)
end

# TODO : Need a similar test of shmem cleanup for OSX

##### SharedArray tests

function check_pids_all(S::SharedArray)
    pidtested = falses(size(S))
    for p in procs(S)
        idxes_in_p = remotecall_fetch(p, S) do D
            parentindexes(D.loc_subarr_1d)[1]
        end
        @test all(sdata(S)[idxes_in_p] .== p)
        pidtested[idxes_in_p] = true
    end
    @test all(pidtested)
end

d = Base.shmem_rand(1:100, dims)
a = convert(Array, d)

partsums = Array{Int}(length(procs(d)))
@sync begin
    for (i, p) in enumerate(procs(d))
        @async partsums[i] = remotecall_fetch(p, d) do D
            sum(D.loc_subarr_1d)
        end
    end
end
@test sum(a) == sum(partsums)

d = Base.shmem_rand(dims)
for p in procs(d)
    idxes_in_p = remotecall_fetch(p, d) do D
        parentindexes(D.loc_subarr_1d)[1]
    end
    idxf = first(idxes_in_p)
    idxl = last(idxes_in_p)
    d[idxf] = Float64(idxf)
    rv = remotecall_fetch(p, d,idxf,idxl) do D,idxf,idxl
        assert(D[idxf] == Float64(idxf))
        D[idxl] = Float64(idxl)
        D[idxl]
    end
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
a = rand(dims)
@test sdata(a) == a

d = SharedArray{Int}(dims, init = D->fill!(D.loc_subarr_1d, myid()))
for p in procs(d)
    idxes_in_p = remotecall_fetch(p, d) do D
        parentindexes(D.loc_subarr_1d)[1]
    end
    idxf = first(idxes_in_p)
    idxl = last(idxes_in_p)
    @test d[idxf] == p
    @test d[idxl] == p
end

d = @inferred(SharedArray{Float64,2}((2,3)))
@test isa(d[:,2], Vector{Float64})

### SharedArrays from a file

# Mapping an existing file
fn = tempname()
write(fn, 1:30)
sz = (6,5)
Atrue = reshape(1:30, sz)

S = @inferred(SharedArray{Int,2}(fn, sz))
@test S == Atrue
@test length(procs(S)) > 1
@everywhere procs(S) begin
    $fill!($S.loc_subarr_1d, $myid())
end
check_pids_all(S)

filedata = similar(Atrue)
read!(fn, filedata)
@test filedata == sdata(S)
finalize(S)

# Error for write-only files
@test_throws ArgumentError SharedArray{Int,2}(fn, sz, mode="w")

# Error for file doesn't exist, but not allowed to create
@test_throws ArgumentError SharedArray{Int,2}(joinpath(tempdir(),randstring()), sz, mode="r")

# Creating a new file
fn2 = tempname()
S = SharedArray{Int,2}(fn2, sz, init=D->D[localindexes(D)] = myid())
@test S == filedata
filedata2 = similar(Atrue)
read!(fn2, filedata2)
@test filedata == filedata2
finalize(S)

# Appending to a file
fn3 = tempname()
write(fn3, ones(UInt8, 4))
S = SharedArray{UInt8}(fn3, sz, 4, mode="a+", init=D->D[localindexes(D)]=0x02)
len = prod(sz)+4
@test filesize(fn3) == len
filedata = Array{UInt8}(len)
read!(fn3, filedata)
@test all(filedata[1:4] .== 0x01)
@test all(filedata[5:end] .== 0x02)
finalize(S)

# call gc 3 times to avoid unlink: operation not permitted (EPERM) on Windows
S = nothing
@everywhere gc()
@everywhere gc()
@everywhere gc()
rm(fn); rm(fn2); rm(fn3)

### Utility functions

# construct PR #13514
S = @inferred(SharedArray{Int}((1,2,3)))
@test size(S) == (1,2,3)
@test typeof(S) <: SharedArray{Int}
S = @inferred(SharedArray{Int}(2))
@test size(S) == (2,)
@test typeof(S) <: SharedArray{Int}
S = @inferred(SharedArray{Int}(1,2))
@test size(S) == (1,2)
@test typeof(S) <: SharedArray{Int}
S = @inferred(SharedArray{Int}(1,2,3))
@test size(S) == (1,2,3)
@test typeof(S) <: SharedArray{Int}

# reshape

d = Base.shmem_fill(1.0, (10,10,10))
@test ones(100, 10) == reshape(d,(100,10))
d = Base.shmem_fill(1.0, (10,10,10))
@test_throws DimensionMismatch reshape(d,(50,))

# rand, randn
d = Base.shmem_rand(dims)
@test size(rand!(d)) == dims
d = Base.shmem_fill(1.0, dims)
@test size(randn!(d)) == dims

# similar
d = Base.shmem_rand(dims)
@test size(similar(d, Complex128)) == dims
@test size(similar(d, dims)) == dims

# issue #6362
d = Base.shmem_rand(dims)
s = copy(sdata(d))
ds = deepcopy(d)
@test ds == d
pids_d = procs(d)
remotecall_fetch(setindex!, pids_d[findfirst(id->(id != myid()), pids_d)], d, 1.0, 1:10)
@test ds != d
@test s != d
copy!(d, s)
@everywhere setid!(A) = A[localindexes(A)] = myid()
@everywhere procs(ds) setid!($ds)
@test d == s
@test ds != s
@test first(ds) == first(procs(ds))
@test last(ds)  ==  last(procs(ds))


# SharedArray as an array
# Since the data in d will depend on the nprocs, just test that these operations work
a = d[1:5]
@test_throws BoundsError d[-1:5]
a = d[1,1,1:3:end]
d[2:4] = 7
d[5,1:2:4,8] = 19

AA = rand(4,2)
A = @inferred(convert(SharedArray, AA))
B = @inferred(convert(SharedArray, AA'))
@test B*A == adjoint(AA)*AA

d=SharedArray{Int64,2}((10,10); init = D->fill!(D.loc_subarr_1d, myid()), pids=[id_me, id_other])
d2 = map(x->1, d)
@test reduce(+, d2) == 100

@test reduce(+, d) == ((50*id_me) + (50*id_other))
map!(x->1, d, d)
@test reduce(+, d) == 100

@test fill!(d, 1) == ones(10, 10)
@test fill!(d, 2.) == fill(2, 10, 10)
@test d[:] == fill(2, 100)
@test d[:,1] == fill(2, 10)
@test d[1,:] == fill(2, 10)

# Boundary cases where length(S) <= length(pids)
@test 2.0 == remotecall_fetch(D->D[2], id_other, Base.shmem_fill(2.0, 2; pids=[id_me, id_other]))
@test 3.0 == remotecall_fetch(D->D[1], id_other, Base.shmem_fill(3.0, 1; pids=[id_me, id_other]))

# Shared arrays of singleton immutables
@everywhere struct ShmemFoo end
for T in [Void, ShmemFoo]
    local s = @inferred(SharedArray{T}(10))
    @test T() === remotecall_fetch(x->x[3], workers()[1], s)
end

# Issue #14664
d = SharedArray{Int}(10)
@sync @parallel for i=1:10
    d[i] = i
end

for (x,i) in enumerate(d)
    @test x == i
end

# complex
sd = SharedArray{Int}(10)
se = SharedArray{Int}(10)
@sync @parallel for i=1:10
    sd[i] = i
    se[i] = i
end
sc = convert(SharedArray, complex.(sd,se))
for (x,i) in enumerate(sc)
    @test i == complex(x,x)
end

# Once finalized accessing remote references and shared arrays should result in exceptions.
function finalize_and_test(r)
    finalize(r)
    @test_throws ErrorException fetch(r)
end

for id in [id_me, id_other]
    local id
    finalize_and_test(Future(id))
    finalize_and_test((r=Future(id); put!(r, 1); r))
    finalize_and_test(RemoteChannel(id))
    finalize_and_test((r=RemoteChannel(id); put!(r, 1); r))
end

d = SharedArray{Int}(10)
finalize(d)
@test_throws BoundsError d[1]

# Issue 22139
let
    aorig = a1 = SharedArray{Float64}((3, 3))
    a1 = remotecall_fetch(fill!, id_other, a1, 1.0)
    @test object_id(aorig) == object_id(a1)
    id = a1.id
    aorig = nothing
    a1 = remotecall_fetch(fill!, id_other, a1, 1.0)
    gc(); gc()
    a1 = remotecall_fetch(fill!, id_other, a1, 1.0)
    @test haskey(Base.sa_refs, id)
    finalize(a1)
    @test !haskey(Base.sa_refs, id)
end

# Test @parallel load balancing - all processors should get either M or M+1
# iterations out of the loop range for some M.
ids = @parallel((a,b)->[a;b], for i=1:7; myid(); end)
workloads = Int[sum(ids .== i) for i in 2:nprocs()]
@test maximum(workloads) - minimum(workloads) <= 1

# @parallel reduction should work even with very short ranges
@test @parallel(+, for i=1:2; i; end) == 3

@test_throws ArgumentError sleep(-1)
@test_throws ArgumentError timedwait(()->false, 0.1, pollint=-0.5)

# specify pids for pmap
@test sort(workers()[1:2]) == sort(unique(pmap(WorkerPool(workers()[1:2]), x->(sleep(0.1);myid()), 1:10)))

# Testing buffered  and unbuffered reads
# This large array should write directly to the socket
a = ones(10^6)
@test a == remotecall_fetch((x)->x, id_other, a)

# Not a bitstype, should be buffered
s = [randstring() for x in 1:10^5]
@test s == remotecall_fetch((x)->x, id_other, s)

#large number of small requests
num_small_requests = 10000
@test fill(id_other, num_small_requests) == [remotecall_fetch(myid, id_other) for i in 1:num_small_requests]

# test parallel sends of large arrays from multiple tasks to the same remote worker
ntasks = 10
rr_list = [Channel(1) for x in 1:ntasks]

for rr in rr_list
    local rr
    let rr = rr
        @async try
            for i in 1:10
                a = rand(2*10^5)
                @test a == remotecall_fetch(x->x, id_other, a)
                yield()
            end
            put!(rr, :OK)
        catch
            put!(rr, :ERROR)
        end
    end
end

@test [fetch(rr) for rr in rr_list] == [:OK for x in 1:ntasks]

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
    close(c)
end

test_channel(Channel(10))
test_channel(RemoteChannel(()->Channel(10)))

c=Channel{Int}(1)
@test_throws MethodError put!(c, "Hello")

# test channel iterations
function test_iteration(in_c, out_c)
    t=@schedule for v in in_c
        put!(out_c, v)
    end

    isa(in_c, Channel) && @test isopen(in_c) == true
    put!(in_c, 1)
    @test take!(out_c) == 1
    put!(in_c, "Hello")
    close(in_c)
    @test take!(out_c) == "Hello"
    isa(in_c, Channel) && @test isopen(in_c) == false
    @test_throws InvalidStateException put!(in_c, :foo)
    yield()
    @test istaskdone(t) == true
end

test_iteration(Channel(10), Channel(10))
# make sure exceptions propagate when waiting on Tasks
@test_throws CompositeException (@sync (@async error("oops")))
try
    @sync begin
        for i in 1:5
            @async error(i)
        end
    end
    error("unexpected")
catch ex
    @test typeof(ex) == CompositeException
    @test length(ex) == 5
    @test typeof(ex.exceptions[1]) == CapturedException
    @test typeof(ex.exceptions[1].ex) == ErrorException
    # test start, next, and done
    for (i, i_ex) in enumerate(ex)
        @test i == parse(Int, i_ex.ex.msg)
    end
    # test showerror
    err_str = sprint(showerror, ex)
    err_one_str = sprint(showerror, ex.exceptions[1])
    @test err_str == err_one_str * "\n\n...and 4 more exception(s).\n"
end
@test sprint(showerror, CompositeException()) == "CompositeException()\n"

function test_remoteexception_thrown(expr)
    try
        expr()
        error("unexpected")
    catch ex
        @test typeof(ex) == RemoteException
        @test typeof(ex.captured) == CapturedException
        @test typeof(ex.captured.ex) == ErrorException
        @test ex.captured.ex.msg == "foobar"
    end
end

for id in [id_other, id_me]
    local id
    test_remoteexception_thrown() do
        remotecall_fetch(id) do
            throw(ErrorException("foobar"))
        end
    end
    test_remoteexception_thrown() do
        remotecall_wait(id) do
            throw(ErrorException("foobar"))
        end
    end
    test_remoteexception_thrown() do
        wait(remotecall(id) do
            throw(ErrorException("foobar"))
        end)
    end
end

# make sure the stackframe from the remote error can be serialized
let ex
    try
        remotecall_fetch(id_other) do
            @eval module AModuleLocalToOther
                foo() = throw(ErrorException("A.error"))
                foo()
            end
        end
    catch ex
    end
    @test (ex::RemoteException).pid == id_other
    @test ((ex.captured::CapturedException).ex::ErrorException).msg == "A.error"
    bt = ex.captured.processed_bt::Array{Any,1}
    @test length(bt) > 1
    frame, repeated = bt[1]::Tuple{StackFrame, Int}
    @test frame.func == :foo
    @test isnull(frame.linfo)
    @test repeated == 1
end

# pmap tests. Needs at least 4 processors dedicated to the below tests. Which we currently have
# since the distributed tests are now spawned as a separate set.

# Test all combinations of pmap keyword args.
pmap_args = [
                (:distributed, [:default, false]),
                (:batch_size, [:default,2]),
                (:on_error, [:default, e -> (e.msg == "foobar" ? true : rethrow(e))]),
                (:retry_delays, [:default, fill(0.001, 1000)]),
                (:retry_check, [:default, (s,e) -> (s,endswith(e.msg,"foobar"))]),
            ]

kwdict = Dict()
function walk_args(i)
    if i > length(pmap_args)
        kwargs = []
        for (k,v) in kwdict
            if v !== :default
                push!(kwargs, (k,v))
            end
        end

        data = 1:100

        testw = kwdict[:distributed] === false ? [1] : workers()

        if kwdict[:retry_delays] !== :default
            mapf = x -> iseven(myid()) ? error("notfoobar") : (x*2, myid())
            results_test = pmap_res -> begin
                results = [x[1] for x in pmap_res]
                pids = [x[2] for x in pmap_res]
                @test results == [2:2:200...]
                for p in testw
                    if isodd(p)
                        @test p in pids
                    else
                        @test !(p in pids)
                    end
                end
            end
        elseif kwdict[:on_error] === :default
            mapf = x -> (x*2, myid())
            results_test = pmap_res -> begin
                results = [x[1] for x in pmap_res]
                pids = [x[2] for x in pmap_res]
                @test results == [2:2:200...]
                for p in testw
                    @test p in pids
                end
            end
        else
            mapf = x -> iseven(x) ? error("foobar") : (x*2, myid())
            results_test = pmap_res -> begin
                w = testw
                for (idx,x) in enumerate(data)
                    if iseven(x)
                        @test pmap_res[idx] == true
                    else
                        @test pmap_res[idx][1] == x*2
                        @test pmap_res[idx][2] in w
                    end
                end
            end
        end

        try
            results_test(pmap(mapf, data; kwargs...))
        catch e
            println("pmap executing with args : ", kwargs)
            rethrow(e)
        end

        return
    end

    kwdict[pmap_args[i][1]] = pmap_args[i][2][1]
    walk_args(i+1)

    kwdict[pmap_args[i][1]] = pmap_args[i][2][2]
    walk_args(i+1)
end

# Start test for various kw arg combinations
walk_args(1)

# Simple test for pmap throws error
let error_thrown = false
    try
        pmap(x -> x == 50 ? error("foobar") : x, 1:100)
    catch e
        @test e.captured.ex.msg == "foobar"
        error_thrown = true
    end
    @test error_thrown
end

# Test pmap with a generator type iterator
@test [1:100...] == pmap(x->x, Base.Generator(x->(sleep(0.0001); x), 1:100))

# Test pgenerate
n = 10
as = [rand(4,4) for i in 1:n]
bs = deepcopy(as)
cs = collect(Base.Distributed.pgenerate(x->(sleep(rand()*0.1); svdfact(x)), bs))
svdas = map(svdfact, as)
for i in 1:n
    @test cs[i][:U] ≈ svdas[i][:U]
    @test cs[i][:S] ≈ svdas[i][:S]
    @test cs[i][:V] ≈ svdas[i][:V]
end

# Test asyncmap
@test allunique(asyncmap(x->(sleep(1.0);object_id(current_task())), 1:10))

# num tasks
@test length(unique(asyncmap(x->(yield();object_id(current_task())), 1:20; ntasks=5))) == 5

# default num tasks
@test length(unique(asyncmap(x->(yield();object_id(current_task())), 1:200))) == 100

# ntasks as a function
let nt=0
    global nt_func
    nt_func() = (v=div(nt, 25); nt+=1; v)  # increment number of tasks by 1 for every 25th call.
                                           # nt_func() will be called initally once and then for every
                                           # iteration
end
@test length(unique(asyncmap(x->(yield();object_id(current_task())), 1:200; ntasks=nt_func))) == 7

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
@test Base.iteratorsize(len_only_iterable) == Base.HasLength()
@test asyncmap(identity, len_only_iterable) == map(identity, len_only_iterable)

# Error conditions
@test_throws ArgumentError asyncmap(identity, 1:10; batch_size=0)
@test_throws ArgumentError asyncmap(identity, 1:10; batch_size="10")
@test_throws ArgumentError asyncmap(identity, 1:10; ntasks="10")

# asyncmap and pmap with various types. Test for equivalence with map
function testmap_equivalence(f, c...)
    x1 = asyncmap(f,c...)
    x2 = pmap(f,c...)
    x3 = map(f,c...)

    if Base.iteratorsize == Base.HasShape()
        @test size(x1) == size(x3)
        @test size(x2) == size(x3)
    else
        @test length(x1) == length(x3)
        @test length(x2) == length(x3)
    end

    @test eltype(x1) == eltype(x3)
    @test eltype(x2) == eltype(x3)

    for (v1,v2) in zip(x1,x3)
        @test v1==v2
    end
    for (v1,v2) in zip(x2,x3)
        @test v1==v2
    end
end

testmap_equivalence(identity, (1,2,3,4))
testmap_equivalence(x->x>0 ? 1.0 : 0.0, sparse(eye(5)))
testmap_equivalence((x,y,z)->x+y+z, 1,2,3)
testmap_equivalence(x->x ? false : true, BitArray(10,10))
testmap_equivalence(x->"foobar", BitArray(10,10))
testmap_equivalence((x,y,z)->string(x,y,z), BitArray(10), ones(10), "1234567890")

@test asyncmap(uppercase, "Hello World!") == map(uppercase, "Hello World!")
@test pmap(uppercase, "Hello World!") == map(uppercase, "Hello World!")


# Test that the default worker pool cycles through all workers
pmap(_->myid(), 1:nworkers())  # priming run
@test nworkers() == length(unique(pmap(_->myid(), 1:100)))

# Test same behaviour when executed on a worker
@test nworkers() == length(unique(remotecall_fetch(()->pmap(_->myid(), 1:100), id_other)))

# Same tests with custom worker pools.
wp = WorkerPool(workers())
@test nworkers() == length(unique(pmap(wp, _->myid(), 1:100)))
@test nworkers() == length(unique(remotecall_fetch(wp->pmap(wp, _->myid(), 1:100), id_other, wp)))


# CachingPool tests
wp = CachingPool(workers())
@test [1:100...] == pmap(wp, x->x, 1:100)

clear!(wp)
@test length(wp.map_obj2ref) == 0

# The below block of tests are usually run only on local development systems, since:
# - tests which print errors
# - addprocs tests are memory intensive
# - ssh addprocs requires sshd to be running locally with passwordless login enabled.
# The test block is enabled by defining env JULIA_TESTFULL=1

DoFullTest = Bool(parse(Int,(get(ENV, "JULIA_TESTFULL", "0"))))

if DoFullTest
    println("Testing exception printing on remote worker from a `remote_do` call")
    println("Please ensure the remote error and backtrace is displayed on screen")

    remote_do(id_other) do
        throw(ErrorException("TESTING EXCEPTION ON REMOTE DO. PLEASE IGNORE"))
    end
    sleep(0.5)  # Give some time for the above error to be printed

    println("\n\nThe following 'invalid connection credentials' error messages are to be ignored.")
    all_w = workers()
    # Test sending fake data to workers. The worker processes will print an
    # error message but should not terminate.
    for w in Base.Distributed.PGRP.workers
        if isa(w, Base.Distributed.Worker)
            local s = connect(get(w.config.host), get(w.config.port))
            write(s, randstring(32))
        end
    end
    @test workers() == all_w
    @test all([p == remotecall_fetch(myid, p) for p in all_w])

if Sys.isunix() # aka have ssh
    function test_n_remove_pids(new_pids)
        for p in new_pids
            w_in_remote = sort(remotecall_fetch(workers, p))
            try
                @test intersect(new_pids, w_in_remote) == new_pids
            catch e
                print("p       :     $p\n")
                print("newpids :     $new_pids\n")
                print("w_in_remote : $w_in_remote\n")
                print("intersect   : $(intersect(new_pids, w_in_remote))\n\n\n")
                rethrow(e)
            end
        end

        remotecall_fetch(rmprocs, 1, new_pids)
    end

    print("\n\nTesting SSHManager. A minimum of 4GB of RAM is recommended.\n")
    print("Please ensure: \n")
    print("1) sshd is running locally with passwordless login enabled.\n")
    print("2) Env variable USER is defined and is the ssh user.\n")
    print("3) Port 9300 is not in use.\n")

    sshflags = `-o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o LogLevel=ERROR `
    #Issue #9951
    hosts=[]
    localhost_aliases = ["localhost", string(getipaddr()), "127.0.0.1"]
    num_workers = parse(Int,(get(ENV, "JULIA_ADDPROCS_NUM", "9")))

    for i in 1:(num_workers/length(localhost_aliases))
        append!(hosts, localhost_aliases)
    end

    print("\nTesting SSH addprocs with $(length(hosts)) workers...\n")
    new_pids = addprocs_with_testenv(hosts; sshflags=sshflags)
    @test length(new_pids) == length(hosts)
    test_n_remove_pids(new_pids)

    print("\nMixed ssh addprocs with :auto\n")
    new_pids = addprocs_with_testenv(["localhost", ("127.0.0.1", :auto), "localhost"]; sshflags=sshflags)
    @test length(new_pids) == (2 + Sys.CPU_CORES)
    test_n_remove_pids(new_pids)

    print("\nMixed ssh addprocs with numeric counts\n")
    new_pids = addprocs_with_testenv([("localhost", 2), ("127.0.0.1", 2), "localhost"]; sshflags=sshflags)
    @test length(new_pids) == 5
    test_n_remove_pids(new_pids)

    print("\nssh addprocs with tunnel\n")
    new_pids = addprocs_with_testenv([("localhost", num_workers)]; tunnel=true, sshflags=sshflags)
    @test length(new_pids) == num_workers
    test_n_remove_pids(new_pids)

    print("\nAll supported formats for hostname\n")
    h1 = "localhost"
    user = ENV["USER"]
    h2 = "$user@$h1"
    h3 = "$h2:22"
    h4 = "$h3 $(string(getipaddr()))"
    h5 = "$h4:9300"

    new_pids = addprocs_with_testenv([h1, h2, h3, h4, h5]; sshflags=sshflags)
    @test length(new_pids) == 5
    test_n_remove_pids(new_pids)

    print("\nkeyword arg exename\n")
    for exename in [`$(joinpath(JULIA_HOME, Base.julia_exename()))`, "$(joinpath(JULIA_HOME, Base.julia_exename()))"]
        for addp_func in [()->addprocs_with_testenv(["localhost"]; exename=exename, exeflags=test_exeflags, sshflags=sshflags),
                          ()->addprocs_with_testenv(1; exename=exename, exeflags=test_exeflags)]

            local new_pids = addp_func()
            @test length(new_pids) == 1
            test_n_remove_pids(new_pids)
        end
    end

end # unix-only
end # full-test

let t = @task 42
    schedule(t, ErrorException(""), error=true)
    @test_throws ErrorException wait(t)
end

# issue #8207
let A = Any[]
    @parallel (+) for i in (push!(A,1); 1:2)
        i
    end
    @test length(A) == 1
end

# issue #13168
function f13168(n)
    val = 0
    for i=1:n val+=sum(rand(n,n)^2) end
    val
end
let t = schedule(@task f13168(100))
    @test t.state == :queued
    @test_throws ErrorException schedule(t)
    yield()
    @test t.state == :done
    @test_throws ErrorException schedule(t)
    @test isa(wait(t),Float64)
end

# issue #13122
@test remotecall_fetch(identity, workers()[1], C_NULL) === C_NULL

# issue #11062
function t11062()
    @async v11062 = 1
    v11062 = 2
end

@test t11062() == 2

# issue #15406
v15406 = remotecall_wait(() -> 1, id_other)
fetch(v15406)
remotecall_wait(fetch, id_other, v15406)

# Test various forms of remotecall* invocations

@everywhere f_args(v1, v2=0; kw1=0, kw2=0) = v1+v2+kw1+kw2

function test_f_args(result, args...; kwargs...)
    @test fetch(remotecall(args...; kwargs...)) == result
    @test fetch(remotecall_wait(args...; kwargs...)) == result
    @test remotecall_fetch(args...; kwargs...) == result

    # A visual test - remote_do should NOT print any errors
    remote_do(args...; kwargs...)
end

for tid in [id_other, id_me, Base.default_worker_pool()]
    test_f_args(1, f_args, tid, 1)
    test_f_args(3, f_args, tid, 1, 2)
    test_f_args(5, f_args, tid, 1; kw1=4)
    test_f_args(13, f_args, tid, 1; kw1=4, kw2=8)
    test_f_args(15, f_args, tid, 1, 2; kw1=4, kw2=8)
end

# Test remote_do
f=Future(id_me)
remote_do(fut->put!(fut, myid()), id_me, f)
@test fetch(f) == id_me

f=Future(id_other)
remote_do(fut->put!(fut, myid()), id_other, f)
@test fetch(f) == id_other

# github PR #14456
n = DoFullTest ? 6 : 5
for i = 1:10^n
    fetch(@spawnat myid() myid())
end

# issue #15451
@test remotecall_fetch(x->(y->2y)(x)+1, workers()[1], 3) == 7

# issue #16091
mutable struct T16091 end
wid = workers()[1]
@test try
    remotecall_fetch(()->T16091, wid)
    false
catch ex
    ((ex::RemoteException).captured::CapturedException).ex === UndefVarError(:T16091)
end
@test try
    remotecall_fetch(identity, wid, T16091)
    false
catch ex
    ((ex::RemoteException).captured::CapturedException).ex === UndefVarError(:T16091)
end

f16091a() = 1
remotecall_fetch(()->eval(:(f16091a() = 2)), wid)
@test remotecall_fetch(f16091a, wid) === 2
@test remotecall_fetch((myid)->remotecall_fetch(f16091a, myid), wid, myid()) === 1

# these will only heisen-fail, since it depends on the gensym counter collisions:
f16091b = () -> 1
remotecall_fetch(()->eval(:(f16091b = () -> 2)), wid)
@test remotecall_fetch(f16091b, 2) === 1
# Global anonymous functions are over-written...
@test remotecall_fetch((myid)->remotecall_fetch(f16091b, myid), wid, myid()) === 1

# ...while local anonymous functions are by definition, local.
let
    f16091c = () -> 1
    @test remotecall_fetch(f16091c, 2) === 1
    @test remotecall_fetch(
        myid -> begin
            let
                f16091c = () -> 2
                remotecall_fetch(f16091c, myid)
            end
        end, wid, myid()) === 2
end

# issue #16451
rng=RandomDevice()
retval = @parallel (+) for _ in 1:10
    rand(rng)
end
@test retval > 0.0 && retval < 10.0

rand(rng)
retval = @parallel (+) for _ in 1:10
    rand(rng)
end
@test retval > 0.0 && retval < 10.0

# serialization tests
wrkr1 = workers()[1]
wrkr2 = workers()[end]

@test remotecall_fetch(p->remotecall_fetch(myid, p), wrkr1, wrkr2) == wrkr2

# Send f to wrkr1 and wrkr2. Then try calling f on wrkr2 from wrkr1
f_myid = ()->myid()
@test wrkr1 == remotecall_fetch(f_myid, wrkr1)
@test wrkr2 == remotecall_fetch(f_myid, wrkr2)
@test wrkr2 == remotecall_fetch((f, p)->remotecall_fetch(f, p), wrkr1, f_myid, wrkr2)

# Deserialization error recovery test
# locally defined module, but unavailable on workers
module LocalFoo
    global foo=1
end

let
    @test_throws RemoteException remotecall_fetch(()->LocalFoo.foo, 2)

    bad_thunk = ()->NonexistantModule.f()
    @test_throws RemoteException remotecall_fetch(bad_thunk, 2)

    # Test that the stream is still usable
    @test remotecall_fetch(()->:test,2) == :test
    ref = remotecall(bad_thunk, 2)
    @test_throws RemoteException fetch(ref)
end

# Test calling @everywhere from a module not defined on the workers
module LocalBar
    bar() = @everywhere new_bar()=myid()
end
LocalBar.bar()
for p in procs()
    @test p == remotecall_fetch(new_bar, p)
end

# @everywhere (remotecall_eval) behaviors (#22589)
let (p, p2) = filter!(p -> p != myid(), procs())
    @test (myid() + 1) == @everywhere myid() (myid() + 1)
    @test (p * 2) == @everywhere p (myid() * 2)
    @test 1 == @everywhere p defined_on_p = 1
    @test !@isdefined defined_on_p
    @test !isdefined(Main, :defined_on_p)
    @test remotecall_fetch(isdefined, p, Main, :defined_on_p)
    @test !remotecall_fetch(isdefined, p2, Main, :defined_on_p)
    @test nothing === @everywhere [p, p] defined_on_p += 1
    @test 3 === @everywhere p defined_on_p
    let ref = Ref(0)
        @test nothing ===
            @everywhere [myid(), p, myid(), myid(), p] begin
                Test.@test Main === @__MODULE__
                $ref[] += 1
            end
        @test ref[] == 3
    end
    function test_throw_on(procs, msg)
        try
            @everywhere procs error($msg)
            error("test failed to throw")
        catch excpt
            if procs isa Int
                ex = Any[excpt]
            else
                ex = Any[ (ex::CapturedException).ex for ex in (excpt::CompositeException).exceptions ]
            end
            for (p, ex) in zip(procs, ex)
                local p
                if procs isa Int || p != myid()
                    @test (ex::RemoteException).pid == p
                    ex = ((ex::RemoteException).captured::CapturedException).ex
                end
                @test (ex::ErrorException).msg == msg
            end
        end
    end
    test_throw_on(p, "everywhere on p")
    test_throw_on(myid(), "everywhere on myid")
    test_throw_on([p, myid()], "everywhere on myid and p")
    test_throw_on([p2, p], "everywhere on p and p2")
end

# Test addprocs enable_threaded_blas parameter

const get_num_threads = function() # anonymous so it will be serialized when called
    blas = BLAS.vendor()
    # Wrap in a try to catch unsupported blas versions
    try
        if blas == :openblas
            return ccall((:openblas_get_num_threads, Base.libblas_name), Cint, ())
        elseif blas == :openblas64
            return ccall((:openblas_get_num_threads64_, Base.libblas_name), Cint, ())
        elseif blas == :mkl
            return ccall((:MKL_Get_Max_Num_Threads, Base.libblas_name), Cint, ())
        end

        # OSX BLAS looks at an environment variable
        if Sys.isapple()
            return ENV["VECLIB_MAXIMUM_THREADS"]
        end
    end

    return nothing
end

function get_remote_num_threads(processes_added)
    return [remotecall_fetch(get_num_threads, proc_id) for proc_id in processes_added]
end

function test_blas_config(pid, expected)
    for worker in Base.Distributed.PGRP.workers
        if worker.id == pid
            @test get(worker.config.enable_threaded_blas) == expected
            return
        end
    end
end

function test_add_procs_threaded_blas()
    if get_num_threads() === nothing
        warn("Skipping blas num threads tests due to unsupported blas version")
        return
    end
    master_blas_thread_count = get_num_threads()

    # Test with default enable_threaded_blas false
    processes_added = addprocs_with_testenv(2)
    for proc_id in processes_added
        test_blas_config(proc_id, false)
    end

    # Master thread should not have changed
    @test get_num_threads() == master_blas_thread_count

    # Threading disabled in children by default
    thread_counts_by_process = get_remote_num_threads(processes_added)
    for thread_count in thread_counts_by_process
        @test thread_count == 1
    end
    rmprocs(processes_added)

    processes_added = addprocs_with_testenv(2, enable_threaded_blas=true)
    for proc_id in processes_added
        test_blas_config(proc_id, true)
    end

    @test get_num_threads() == master_blas_thread_count

    # BLAS.set_num_threads(`num`) doesn't  cause get_num_threads to return `num`
    # depending on the machine, the BLAS version, and BLAS configuration, so
    # we need a very lenient test.
    thread_counts_by_process = get_remote_num_threads(processes_added)
    for thread_count in thread_counts_by_process
        @test thread_count >= 1
    end
    rmprocs(processes_added)
end
test_add_procs_threaded_blas()

#19687
# ensure no race conditions between rmprocs and addprocs
for i in 1:5
    p = addprocs_with_testenv(1)[1]
    @spawnat p sleep(5)
    rmprocs(p; waitfor=0)
end

# Test if a wait has been called on rmprocs(...;waitfor=0), further remotecalls
# don't throw errors.
for i in 1:5
    p = addprocs_with_testenv(1)[1]
    np = nprocs()
    @spawnat p sleep(5)
    wait(rmprocs(p; waitfor=0))
    for pid in procs()
        @test pid == remotecall_fetch(myid, pid)
    end
    @test nprocs() == np - 1
end

# Test that an exception is thrown if workers are unable to be removed within requested time.
if DoFullTest
    pids=addprocs_with_testenv(4);
    @test_throws ErrorException rmprocs(pids; waitfor=0.001);
    # wait for workers to be removed
    while any(x -> (x in procs()), pids)
        sleep(0.1)
    end
end

# Test addprocs/rmprocs from master node only
for f in [ ()->addprocs(1; exeflags=test_exeflags), ()->rmprocs(workers()) ]
    local f
    try
        remotecall_fetch(f, id_other)
        error("Unexpected")
    catch ex
        @test isa(ex, RemoteException)
        @test ex.captured.ex.msg == "Only process 1 can add and remove workers"
    end
end

# Test the following addprocs error conditions
# - invalid host name - github issue #20372
# - julia exe exiting with an error
# - timeout reading host:port from worker STDOUT
# - host:port not found in worker STDOUT in the first 1000 lines

struct ErrorSimulator <: ClusterManager
    mode
end

function Base.launch(manager::ErrorSimulator, params::Dict, launched::Array, c::Condition)
    exename = params[:exename]
    dir = params[:dir]

    cmd = `$(Base.julia_cmd(exename)) --startup-file=no`
    if manager.mode == :timeout
        cmd = `$cmd -e "sleep(10)"`
    elseif manager.mode == :ntries
        cmd = `$cmd -e "[println(x) for x in 1:1001]"`
    elseif manager.mode == :exit
        cmd = `$cmd -e "exit(-1)"`
    else
        error("Unknown mode")
    end
    io = open(detach(setenv(cmd, dir=dir)))

    wconfig = WorkerConfig()
    wconfig.process = io
    wconfig.io = io.out
    push!(launched, wconfig)
    notify(c)
end

testruns = Any[]

if DoFullTest
    append!(testruns, [(()->addprocs_with_testenv(["errorhost20372"]), "Unable to read host:port string from worker. Launch command exited with error?", ())])
end

append!(testruns, [
    (()->addprocs_with_testenv(ErrorSimulator(:exit)), "Unable to read host:port string from worker. Launch command exited with error?", ()),
    (()->addprocs_with_testenv(ErrorSimulator(:ntries)), "Unexpected output from worker launch command. Host:port string not found.", ()),
    (()->addprocs_with_testenv(ErrorSimulator(:timeout)), "Timed out waiting to read host:port string from worker.", ("JULIA_WORKER_TIMEOUT"=>"1",))
])

for (addp_testf, expected_errstr, env) in testruns
    try
        withenv(env...) do
            addp_testf()
        end
        error("Unexpected")
    catch ex
        @test isa(ex, CompositeException)
        @test ex.exceptions[1].ex.msg == expected_errstr
    end
end


# Auto serialization of globals from Main.
# bitstypes
global v1 = 1
@test remotecall_fetch(()->v1, id_other) == v1
@test remotecall_fetch(()->isdefined(Main, :v1), id_other)
for i in 2:5
    global v1 = i
    @test remotecall_fetch(()->v1, id_other) == i
end

# non-bitstypes
global v2 = zeros(10)
for i in 1:5
    v2[i] = i
    @test remotecall_fetch(()->v2, id_other) == v2
end

# Different global bindings to the same object
global v3 = ones(10)
global v4 = v3
@test remotecall_fetch(()->v3, id_other) == remotecall_fetch(()->v4, id_other)
@test remotecall_fetch(()->isdefined(Main, :v3), id_other)
@test remotecall_fetch(()->isdefined(Main, :v4), id_other)

# Global references to Types and Modules should work if they are locally defined
global v5 = Int
global v6 = Base.Distributed
@test remotecall_fetch(()->v5, id_other) === Int
@test remotecall_fetch(()->v6, id_other) === Base.Distributed

struct FooStructLocal end
module FooModLocal end
v5 = FooStructLocal
v6 = FooModLocal
@test_throws RemoteException remotecall_fetch(()->v5, id_other)
@test_throws RemoteException remotecall_fetch(()->v6, id_other)

@everywhere struct FooStructEverywhere end
@everywhere module FooModEverywhere end
v5 = FooStructEverywhere
v6 = FooModEverywhere
@test remotecall_fetch(()->v5, id_other) === FooStructEverywhere
@test remotecall_fetch(()->v6, id_other) === FooModEverywhere


# Test that a global is not being repeatedly serialized when
# a) referenced multiple times in the closure
# b) hash value has not changed.

@everywhere begin
    global testsercnt_d = Dict()
    mutable struct TestSerCnt
        v
    end
    import Base.hash, Base.==
    hash(x::TestSerCnt, h::UInt) = hash(hash(x.v), h)
    ==(x1::TestSerCnt, x2::TestSerCnt) = (x1.v == x2.v)

    function Base.serialize(s::AbstractSerializer, t::TestSerCnt)
        Base.Serializer.serialize_type(s, TestSerCnt)
        serialize(s, t.v)
        global testsercnt_d
        cnt = get!(testsercnt_d, object_id(t), 0)
        testsercnt_d[object_id(t)] = cnt+1
    end

    Base.deserialize(s::AbstractSerializer, ::Type{TestSerCnt}) = TestSerCnt(deserialize(s))
end

# hash value of tsc is not changed
global tsc = TestSerCnt(zeros(10))
for i in 1:5
    remotecall_fetch(()->tsc, id_other)
end
# should have been serialized only once
@test testsercnt_d[object_id(tsc)] == 1

# hash values are changed
n=5
testsercnt_d[object_id(tsc)] = 0
for i in 1:n
    tsc.v[i] = i
    remotecall_fetch(()->tsc, id_other)
end
# should have been serialized as many times as the loop
@test testsercnt_d[object_id(tsc)] == n

# Multiple references in a closure should be serialized only once.
global mrefs = TestSerCnt(ones(10))
@test remotecall_fetch(()->(mrefs.v, 2*mrefs.v, 3*mrefs.v), id_other) == (ones(10), 2*ones(10), 3*ones(10))
@test testsercnt_d[object_id(mrefs)] == 1


# nested anon functions
global f1 = x->x
global f2 = x->f1(x)
v = rand()
@test remotecall_fetch(f2, id_other, v) == v
@test remotecall_fetch(x->f2(x), id_other, v) == v

# consts
const c1 = ones(10)
@test remotecall_fetch(()->c1, id_other) == c1
@test remotecall_fetch(()->isconst(Main, :c1), id_other)

# Test same calls with local vars
function wrapped_var_ser_tests()
    # bitstypes
    local lv1 = 1
    @test remotecall_fetch(()->lv1, id_other) == lv1
    @test !remotecall_fetch(()->isdefined(Main, :lv1), id_other)
    for i in 2:5
        lv1 = i
        @test remotecall_fetch(()->lv1, id_other) == i
    end

    # non-bitstypes
    local lv2 = zeros(10)
    for i in 1:5
        lv2[i] = i
        @test remotecall_fetch(()->lv2, id_other) == lv2
    end

    # nested anon functions
    local lf1 = x->x
    local lf2 = x->lf1(x)
    v = rand()
    @test remotecall_fetch(lf2, id_other, v) == v
    @test remotecall_fetch(x->lf2(x), id_other, v) == v
end

wrapped_var_ser_tests()

# Test internal data structures being cleaned up upon gc.
global ids_cleanup = ones(6)
global ids_func = ()->ids_cleanup

clust_ser = (Base.Distributed.worker_from_id(id_other)).w_serializer
@test remotecall_fetch(ids_func, id_other) == ids_cleanup

@test haskey(clust_ser.glbs_sent, object_id(ids_cleanup))
finalize(ids_cleanup)
@test !haskey(clust_ser.glbs_sent, object_id(ids_cleanup))

# TODO Add test for cleanup from `clust_ser.glbs_in_tnobj`

# reported github issues - Mostly tests with globals and various distributed macros
#2669, #5390
v2669=10
@test fetch(@spawn (1+v2669)) == 11

#12367
refs = []
if true
    n = 10
    for p in procs()
        push!(refs, @spawnat p begin
            @sync for i in 1:n
                nothing
            end
        end)
    end
end
foreach(wait, refs)

#14399
s = convert(SharedArray, [1,2,3,4])
@test pmap(i->length(s), 1:2) == [4,4]

#6760
if true
    a = 2
    x = @parallel (vcat) for k=1:2
        sin(a)
    end
end
@test x == map(_->sin(2), 1:2)

let thrown = false
    try
        remotecall_fetch(sqrt, 2, -1)
    catch e
        thrown = true
        local b = IOBuffer()
        showerror(b, e)
        @test contains(String(take!(b)), "sqrt will only return")
    end
    @test thrown
end

#19463
function foo19463()
    w1 = workers()[1]
    w2 = workers()[2]
    w3 = workers()[3]

    b1 = () -> 1
    b2 = () -> fetch(@spawnat w1 b1()) + 1
    b3 = () -> fetch(@spawnat w2 b2()) + 1
    b4 = () -> fetch(@spawnat w3 b3()) + 1
    b4()
end
@test foo19463() == 4

# Testing clear!
function setup_syms(n, pids)
    syms = []
    for i in 1:n
        symstr = string("clrtest", randstring())
        sym = Symbol(symstr)
        eval(:(global $sym = rand()))
        for p in pids
            eval(:(@test $sym == remotecall_fetch(()->$sym, $p)))
            eval(:(@test remotecall_fetch(isdefined, $p, Main, Symbol($symstr))))
        end
        push!(syms, sym)
    end
    syms
end

function test_clear(syms, pids)
    for p in pids
        for sym in syms
            remote_val = remotecall_fetch(()->getfield(Main, sym), p)
            @test remote_val === nothing
            @test remote_val != getfield(Main, sym)
        end
    end
end

syms = setup_syms(1, [id_other])
clear!(syms[1], id_other)
test_clear(syms, [id_other])

syms = setup_syms(1, workers())
clear!(syms[1], workers())
test_clear(syms, workers())

syms = setup_syms(3, [id_other])
clear!(syms, id_other)
test_clear(syms, [id_other])

syms = setup_syms(3, workers())
clear!(syms, workers())
test_clear(syms, workers())

# Test partial recovery from a deserialization error in CapturedException
try
    expr = quote
                mutable struct DontExistOn1
                    x
                end
                throw(BoundsError(DontExistOn1(1), 1))
           end

    remotecall_fetch(()->eval(expr), id_other)
    error("unexpected")
catch ex
    @test isa(ex.captured.ex.exceptions[1].ex, ErrorException)
    @test contains(ex.captured.ex.exceptions[1].ex.msg, "BoundsError")
    @test ex.captured.ex.exceptions[2].ex == UndefVarError(:DontExistOn1)
end

@test let
    # creates a new worker in the same folder and tries to include file
    tmp_file, temp_file_stream = mktemp()
    close(temp_file_stream)
    tmp_file = relpath(tmp_file)
    try
        proc = addprocs_with_testenv(1)
        include(tmp_file)
        remotecall_fetch(include, proc[1], tmp_file)
        rmprocs(proc)
        rm(tmp_file)
        return true
    catch e
        println(e)
        rm(tmp_file, force=true)
        return false
    end
end == true

let
    # creates a new worker in a different folder and tries to include file
    tmp_dir = mktempdir()
    tmp_dir2 = joinpath(tmp_dir, "2")
    tmp_file = joinpath(tmp_dir2, "testfile")
    tmp_file2 = joinpath(tmp_dir2, "testfile2")
    proc = addprocs_with_testenv(1, dir=tmp_dir)
    try
        mkdir(tmp_dir2)
        write(tmp_file, "23.32 + 32 + myid() + include(\"testfile2\")")
        write(tmp_file2, "myid() * 2")
        @test_throws(ErrorException("could not open file $(abspath("testfile"))"),
                     include("testfile"))
        @test_throws(ErrorException("could not open file $(abspath("testfile2"))"),
                     include("testfile2"))
        @test_throws(ErrorException("could not open file $(abspath("2", "testfile"))"),
                     include(joinpath("2", "testfile")))
        @test include(tmp_file) == 58.32
        @test remotecall_fetch(include, proc[1], joinpath("2", "testfile")) == 55.32 + proc[1] * 3
    finally
        rmprocs(proc)
        rm(tmp_file, force=true)
        rm(tmp_file2, force=true)
        rm(tmp_dir2, force=true)
        rm(tmp_dir, force=true)
    end
end
# cookie and comand line option `--worker` tests. remove workers, set cookie and test
struct WorkerArgTester <: ClusterManager
    worker_opt
    write_cookie
end

function Base.launch(manager::WorkerArgTester, params::Dict, launched::Array, c::Condition)
    dir = params[:dir]
    exename = params[:exename]
    exeflags = params[:exeflags]

    cmd = `$exename $exeflags --bind-to $(Base.Distributed.LPROC.bind_addr) $(manager.worker_opt)`
    cmd = pipeline(detach(setenv(cmd, dir=dir)))
    io = open(cmd, "r+")
    manager.write_cookie && Base.Distributed.write_cookie(io)

    wconfig = WorkerConfig()
    wconfig.process = io
    wconfig.io = io.out
    push!(launched, wconfig)

    notify(c)
end
Base.manage(::WorkerArgTester, ::Integer, ::WorkerConfig, ::Symbol) = nothing

nprocs()>1 && rmprocs(workers())

npids = addprocs_with_testenv(WorkerArgTester(`--worker`, true))
@test remotecall_fetch(myid, npids[1]) == npids[1]
rmprocs(npids)

Base.cluster_cookie("")  # An empty string is a valid cookie
npids = addprocs_with_testenv(WorkerArgTester(`--worker=`, false))
@test remotecall_fetch(myid, npids[1]) == npids[1]
rmprocs(npids)

Base.cluster_cookie("foobar") # custom cookie
npids = addprocs_with_testenv(WorkerArgTester(`--worker=foobar`, false))
@test remotecall_fetch(myid, npids[1]) == npids[1]

# Issue # 22865
# Must be run on a new cluster, i.e., all workers must be in the same state.
rmprocs(workers())
p1,p2 = addprocs_with_testenv(2)
@everywhere f22865(p) = remotecall_fetch(x->x.*2, p, ones(2))
@test ones(2).*2 == remotecall_fetch(f22865, p1, p2)

function reuseport_tests()
    # Run the test on all processes.
    results = asyncmap(procs()) do p
        remotecall_fetch(p) do
            ports_lower = []        # ports of pids lower than myid()
            ports_higher = []       # ports of pids higher than myid()
            for w in Base.Distributed.PGRP.workers
                w.id == myid() && continue
                port = Base._sockname(w.r_stream, true)[2]
                if (w.id == 1)
                    # master connects to workers
                    push!(ports_higher, port)
                elseif w.id < myid()
                    push!(ports_lower, port)
                elseif w.id > myid()
                    push!(ports_higher, port)
                end
            end
            @assert (length(ports_lower) + length(ports_higher)) == nworkers()
            for portset in [ports_lower, ports_higher]
                if (length(portset) > 0) && (length(unique(portset)) != 1)
                    warn("SO_REUSEPORT TESTS FAILED. UNSUPPORTED/OLDER UNIX VERSION?")
                    return 0
                end
            end
            return myid()
        end
    end

    # Ensure that the code has indeed been successfully executed everywhere
    @test all(p -> p in results, procs())
end

# Test that the client port is reused. SO_REUSEPORT may not be supported on
# all UNIX platforms, Linux kernels prior to 3.9 and older versions of OSX
if ccall(:jl_has_so_reuseport, Int32, ()) == 1
    rmprocs(workers())
    addprocs_with_testenv(4; lazy=false)
    reuseport_tests()
else
    info("SO_REUSEPORT is unsupported, skipping reuseport tests.")
end

# Run topology tests last after removing all workers, since a given
# cluster at any time only supports a single topology.
rmprocs(workers())
include("topology.jl")
