# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

inline_flag = Base.JLOptions().can_inline == 1 ? `` : `--inline=no`
cov_flag = ``
if Base.JLOptions().code_coverage == 1
    cov_flag = `--code-coverage=user`
elseif Base.JLOptions().code_coverage == 2
    cov_flag = `--code-coverage=all`
end

# Test a few "remote" invocations when no workers are present
@test remote(myid)() == 1
@test pmap(identity, 1:100) == [1:100...]
@test 100 == @parallel (+) for i in 1:100
        1
    end

addprocs(4; exeflags=`$cov_flag $inline_flag --check-bounds=yes --startup-file=no --depwarn=error`)

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


id_me = myid()
id_other = filter(x -> x != id_me, procs())[rand(1:(nprocs()-1))]

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
    @test remotecall_fetch(k->(yield();haskey(Base.PGRP.refs, k)), id, fid) == true
    @test isnull(f.v) == true
    @test fetch(f) == id
    @test isnull(f.v) == false
    @test remotecall_fetch(k->(yield();haskey(Base.PGRP.refs, k)), id, fid) == false


    # if unfetched, it should be deleted after a finalize
    f = remotecall(myid, id)
    fid = Base.remoteref_id(f)
    @test remotecall_fetch(k->(yield();haskey(Base.PGRP.refs, k)), id, fid) == true
    @test isnull(f.v) == true
    finalize(f)
    Base.flush_gc_msgs()
    @test remotecall_fetch(k->(yield();haskey(Base.PGRP.refs, k)), id, fid) == false
end

test_futures_dgc(id_me)
test_futures_dgc(id_other)

# if sent to another worker, it should not be deleted till the other worker has fetched.
wid1 = workers()[1]
wid2 = workers()[2]
f = remotecall(myid, wid1)
fid = Base.remoteref_id(f)

fstore = RemoteChannel(wid2)
put!(fstore, f)

@test fetch(f) == wid1
@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, fid) == true
remotecall_fetch(r->fetch(fetch(r)), wid2, fstore)
@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, fid) == false

# put! should release remote reference since it would have been cached locally
f = Future(wid1)
fid = Base.remoteref_id(f)

# should not be created remotely till accessed
@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, fid) == false
# create it remotely
isready(f)

@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, fid) == true
put!(f, :OK)
@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, fid) == false
@test fetch(f) == :OK

# RemoteException should be thrown on a put! when another process has set the value
f = Future(wid1)
fid = Base.remoteref_id(f)

fstore = RemoteChannel(wid2)
put!(fstore, f) # send f to wid2
put!(f, :OK) # set value from master

@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, fid) == true

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
    @test remotecall_fetch(k->(yield();haskey(Base.PGRP.refs, k)), id, rrid) == true
    @test fetch(rr) == :OK
    @test remotecall_fetch(k->(yield();haskey(Base.PGRP.refs, k)), id, rrid) == true
    finalize(rr)
    Base.flush_gc_msgs()
    @test remotecall_fetch(k->(yield();haskey(Base.PGRP.refs, k)), id, rrid) == false
end
test_remoteref_dgc(id_me)
test_remoteref_dgc(id_other)

# if sent to another worker, it should not be deleted till the other worker has also finalized.
wid1 = workers()[1]
wid2 = workers()[2]
rr = RemoteChannel(wid1)
rrid = Base.remoteref_id(rr)

fstore = RemoteChannel(wid2)
put!(fstore, rr)

@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, rrid) == true
finalize(rr); Base.flush_gc_msgs() # finalize locally
@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, rrid) == true
remotecall_fetch(r->(finalize(take!(r)); Base.flush_gc_msgs(); nothing), wid2, fstore) # finalize remotely
sleep(0.5) # to ensure that wid2 messages have been executed on wid1
@test remotecall_fetch(k->haskey(Base.PGRP.refs, k), wid1, rrid) == false

@test fetch(@spawnat id_other myid()) == id_other
@test @fetchfrom id_other begin myid() end == id_other
@fetch begin myid() end

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

dims = (20,20,20)

if is_linux()
    S = SharedArray(Int64, dims)
    @test startswith(S.segname, "/jl")
    @test !ispath("/dev/shm" * S.segname)

    S = SharedArray(Int64, dims; pids=[id_other])
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

d = SharedArray(Int, dims, init = D->fill!(D.loc_subarr_1d, myid()))
for p in procs(d)
    idxes_in_p = remotecall_fetch(p, d) do D
        parentindexes(D.loc_subarr_1d)[1]
    end
    idxf = first(idxes_in_p)
    idxl = last(idxes_in_p)
    @test d[idxf] == p
    @test d[idxl] == p
end

d = @inferred(SharedArray(Float64, (2,3)))
@test isa(d[:,2], Vector{Float64})

### SharedArrays from a file

# Mapping an existing file
fn = tempname()
write(fn, 1:30)
sz = (6,5)
Atrue = reshape(1:30, sz)

S = @inferred(SharedArray(fn, Int, sz))
@test S == Atrue
@test length(procs(S)) > 1
@sync begin
    for p in procs(S)
        @async remotecall_wait(p, S) do D
            fill!(D.loc_subarr_1d, myid())
        end
    end
end
check_pids_all(S)

filedata = similar(Atrue)
read!(fn, filedata)
@test filedata == sdata(S)
finalize(S)

# Error for write-only files
@test_throws ArgumentError SharedArray(fn, Int, sz, mode="w")

# Error for file doesn't exist, but not allowed to create
@test_throws ArgumentError SharedArray(joinpath(tempdir(),randstring()), Int, sz, mode="r")

# Creating a new file
fn2 = tempname()
S = SharedArray(fn2, Int, sz, init=D->D[localindexes(D)] = myid())
@test S == filedata
filedata2 = similar(Atrue)
read!(fn2, filedata2)
@test filedata == filedata2
finalize(S)

# Appending to a file
fn3 = tempname()
write(fn3, ones(UInt8, 4))
S = SharedArray(fn3, UInt8, sz, 4, mode="a+", init=D->D[localindexes(D)]=0x02)
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
@sync for p in procs(ds)
    @async remotecall_wait(setid!, p, ds)
end
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
@test B*A == ctranspose(AA)*AA

d=SharedArray(Int64, (10,10); init = D->fill!(D.loc_subarr_1d, myid()), pids=[id_me, id_other])
d2 = map(x->1, d)
@test reduce(+, d2) == 100

@test reduce(+, d) == ((50*id_me) + (50*id_other))
map!(x->1, d)
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
@everywhere immutable ShmemFoo end
for T in [Void, ShmemFoo]
    s = @inferred(SharedArray(T, 10))
    @test T() === remotecall_fetch(x->x[3], workers()[1], s)
end

# Issue #14664
d = SharedArray(Int,10)
@sync @parallel for i=1:10
    d[i] = i
end

for (x,i) in enumerate(d)
    @test x == i
end

# complex
sd = SharedArray(Int,10)
se = SharedArray(Int,10)
@sync @parallel for i=1:10
    sd[i] = i
    se[i] = i
end
sc = complex(sd,se)
for (x,i) in enumerate(sc)
    @test i == complex(x,x)
end

# Once finalized accessing remote references and shared arrays should result in exceptions.
function finalize_and_test(r)
    finalize(r)
    @test_throws ErrorException fetch(r)
end

for id in [id_me, id_other]
    finalize_and_test(Future(id))
    finalize_and_test((r=Future(id); put!(r, 1); r))
    finalize_and_test(RemoteChannel(id))
    finalize_and_test((r=RemoteChannel(id); put!(r, 1); r))
end

d = SharedArray(Int,10)
finalize(d)
@test_throws BoundsError d[1]


# Test @parallel load balancing - all processors should get either M or M+1
# iterations out of the loop range for some M.
ids = @parallel((a,b)->[a;b], for i=1:7; myid(); end)
workloads = Int[sum(ids .== i) for i in 2:nprocs()]
@test maximum(workloads) - minimum(workloads) <= 1

# @parallel reduction should work even with very short ranges
@test @parallel(+, for i=1:2; i; end) == 3

# Testing timedwait on multiple channels
@sync begin
    rr1 = Channel()
    rr2 = Channel()
    rr3 = Channel()

    callback() = all(map(isready, [rr1, rr2, rr3]))
    # precompile functions which will be tested for execution time
    @test !callback()
    @test timedwait(callback, 0.0) === :timed_out

    @async begin sleep(0.5); put!(rr1, :ok) end
    @async begin sleep(1.0); put!(rr2, :ok) end
    @async begin sleep(2.0); put!(rr3, :ok) end

    tic()
    timedwait(callback, 1.0)
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

# Test multiple concurrent put!/take! on a channel
function testcpt()
    c = Channel()
    size = 0
    inc() = size += 1
    dec() = size -= 1
    @sync for i = 1:10^4
        @async (sleep(rand()); put!(c, i); inc())
        @async (sleep(rand()); take!(c); dec())
    end
    @test size == 0
end
testcpt()

# Test multiple "for" loops waiting on the same channel which
# is closed after adding a few elements.
c=Channel()
results=[]
@sync begin
    for i in 1:20
        @async for i in c
            push!(results, i)
        end
    end
    sleep(1.0)
    for i in 1:5
        put!(c,i)
    end
    close(c)
end
@test sum(results) == 15

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
rr_list = [Channel() for x in 1:ntasks]
a = ones(2*10^5)
for rr in rr_list
    @async let rr=rr
        try
            for i in 1:10
                @test a == remotecall_fetch((x)->x, id_other, a)
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
    errors = map(x->x.ex.msg, ex.exceptions)
    @test collect(1:5) == sort(map(x->parse(Int, x), errors))
end

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
# since the parallel tests are now spawned as a separate set.
function unmangle_exception(e)
    while any(x->isa(e, x), [CompositeException, RemoteException, CapturedException])
        if isa(e, CompositeException)
            e = e.exceptions[1].ex
        end
        if isa(e, RemoteException)
            e = e.captured.ex
        end
        if isa(e, CapturedException)
            e = e.ex
        end
    end
    return e
end

# Test all combinations of pmap keyword args.
pmap_args = [
                (:distributed, [:default, false]),
                (:batch_size, [:default,2]),
                (:on_error, [:default, e -> unmangle_exception(e).msg == "foobar"]),
                (:retry_on, [:default, e -> unmangle_exception(e).msg == "foobar"]),
                (:retry_n, [:default, typemax(Int)-1]),
                (:retry_max_delay, [0, 0.001])
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

        data = [1:100...]

        testw = kwdict[:distributed] === false ? [1] : workers()

        if (kwdict[:on_error] === :default) && (kwdict[:retry_n] === :default)
            mapf = x -> (x*2, myid())
            results_test = pmap_res -> begin
                results = [x[1] for x in pmap_res]
                pids = [x[2] for x in pmap_res]
                @test results == [2:2:200...]
                for p in testw
                    @test p in pids
                end
            end
        elseif kwdict[:retry_n] !== :default
            mapf = x -> iseven(myid()) ? error("foobar") : (x*2, myid())
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
        else (kwdict[:on_error] !== :default) && (kwdict[:retry_n] === :default)
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
error_thrown = false
try
    pmap(x -> x==50 ? error("foobar") : x, 1:100)
catch e
    @test unmangle_exception(e).msg == "foobar"
    error_thrown = true
end
@test error_thrown

# Test pmap with a generator type iterator
@test [1:100...] == pmap(x->x, Base.Generator(x->(sleep(0.0001); x), 1:100))

# Test asyncmap
@test allunique(asyncmap(x->(sleep(1.0);object_id(current_task())), 1:10))

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
    # Topology tests need to run externally since a given cluster at any
    # time can only support a single topology and the current session
    # is already running in parallel under the default topology.
    script = joinpath(dirname(@__FILE__), "topology.jl")
    cmd = `$(Base.julia_cmd()) $script`

    (strm, proc) = open(pipeline(cmd, stderr=STDERR))
    wait(proc)
    if !success(proc) && ccall(:jl_running_on_valgrind,Cint,()) == 0
        println(readstring(strm))
        error("Topology tests failed : $cmd")
    end

    println("Testing exception printing on remote worker from a `remote_do` call")
    println("Please ensure the remote error and backtrace is displayed on screen")

    Base.remote_do(id_other) do
        throw(ErrorException("TESTING EXCEPTION ON REMOTE DO. PLEASE IGNORE"))
    end
    sleep(0.5)  # Give some time for the above error to be printed

    println("\n\nThe following 'invalid connection credentials' error messages are to be ignored.")
    all_w = workers()
    # Test sending fake data to workers. The worker processes will print an
    # error message but should not terminate.
    for w in Base.PGRP.workers
        if isa(w, Base.Worker)
            s = connect(get(w.config.host), get(w.config.port))
            write(s, randstring(32))
        end
    end
    @test workers() == all_w
    @test all([p == remotecall_fetch(myid, p) for p in all_w])

if is_unix() # aka have ssh
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

        @test :ok == remotecall_fetch(1, new_pids) do p
            rmprocs(p; waitfor=5.0)
        end
    end

    print("\n\nTesting SSHManager. A minimum of 4GB of RAM is recommended.\n")
    print("Please ensure sshd is running locally with passwordless login enabled.\n")

    sshflags = `-o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o LogLevel=ERROR `
    #Issue #9951
    hosts=[]
    localhost_aliases = ["localhost", string(getipaddr()), "127.0.0.1"]
    num_workers = parse(Int,(get(ENV, "JULIA_ADDPROCS_NUM", "9")))

    for i in 1:(num_workers/length(localhost_aliases))
        append!(hosts, localhost_aliases)
    end

    print("\nTesting SSH addprocs with $(length(hosts)) workers...\n")
    new_pids = remotecall_fetch(1, hosts, sshflags) do h, sf
        addprocs(h; sshflags=sf)
    end
    @test length(new_pids) == length(hosts)
    test_n_remove_pids(new_pids)

    print("\nMixed ssh addprocs with :auto\n")
    new_pids = sort(remotecall_fetch(1, ["localhost", ("127.0.0.1", :auto), "localhost"], sshflags) do h, sf
        addprocs(h; sshflags=sf)
    end)
    @test length(new_pids) == (2 + Sys.CPU_CORES)
    test_n_remove_pids(new_pids)

    print("\nMixed ssh addprocs with numeric counts\n")
    new_pids = sort(remotecall_fetch(1, [("localhost", 2), ("127.0.0.1", 2), "localhost"], sshflags) do h, sf
        addprocs(h; sshflags=sf)
    end)
    @test length(new_pids) == 5
    test_n_remove_pids(new_pids)

    print("\nssh addprocs with tunnel\n")
    new_pids = sort(remotecall_fetch(1, [("localhost", num_workers)], sshflags) do h, sf
        addprocs(h; tunnel=true, sshflags=sf)
    end)
    @test length(new_pids) == num_workers
    test_n_remove_pids(new_pids)
end # unix-only
end # full-test

# issue #7727
let A = [], B = []
    t = @task produce(11)
    @sync begin
        @async for x in t; push!(A,x); end
        @async for x in t; push!(B,x); end
    end
    @test (A == [11]) != (B == [11])
end

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
    !isa(args[2], WorkerPool) && Base.remote_do(args...; kwargs...)
end

for tid in [id_other, id_me, Base.default_worker_pool()]
    test_f_args(1, f_args, tid, 1)
    test_f_args(3, f_args, tid, 1, 2)
    test_f_args(5, f_args, tid, 1; kw1=4)
    test_f_args(13, f_args, tid, 1; kw1=4, kw2=8)
    test_f_args(15, f_args, tid, 1, 2; kw1=4, kw2=8)
end

# github PR #14456
n = DoFullTest ? 6 : 5
for i = 1:10^n
    fetch(@spawnat myid() myid())
end

# issue #15451
@test remotecall_fetch(x->(y->2y)(x)+1, workers()[1], 3) == 7

# issue #16091
type T16091 end
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
@test remotecall_fetch((myid)->remotecall_fetch(f16091b, myid), wid, myid()) === 2


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
        if is_apple()
            return ENV["VECLIB_MAXIMUM_THREADS"]
        end
    end

    return nothing
end

function get_remote_num_threads(processes_added)
    return [remotecall_fetch(get_num_threads, proc_id) for proc_id in processes_added]
end

function test_blas_config(pid, expected)
    for worker in Base.PGRP.workers
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
    processes_added = addprocs(2)
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

    processes_added = addprocs(2, enable_threaded_blas=true)
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
