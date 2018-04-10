# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Distributed, SharedArrays, Random
include(joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testenv.jl"))

addprocs_with_testenv(4)
@test nprocs() == 5

@everywhere using Test, SharedArrays

id_me = myid()
id_other = filter(x -> x != id_me, procs())[rand(1:(nprocs()-1))]

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
            parentindices(D.loc_subarr_1d)[1]
        end
        @test all(sdata(S)[idxes_in_p] .== p)
        pidtested[idxes_in_p] = true
    end
    @test all(pidtested)
end

d = SharedArrays.shmem_rand(1:100, dims)
a = convert(Array, d)

partsums = Vector{Int}(undef, length(procs(d)))
@sync begin
    for (i, p) in enumerate(procs(d))
        @async partsums[i] = remotecall_fetch(p, d) do D
            sum(D.loc_subarr_1d)
        end
    end
end
@test sum(a) == sum(partsums)

d = SharedArrays.shmem_rand(dims)
for p in procs(d)
    idxes_in_p = remotecall_fetch(p, d) do D
        parentindices(D.loc_subarr_1d)[1]
    end
    idxf = first(idxes_in_p)
    idxl = last(idxes_in_p)
    d[idxf] = Float64(idxf)
    rv = remotecall_fetch(p, d,idxf,idxl) do D,idxf,idxl
        @assert D[idxf] == Float64(idxf)
        D[idxl] = Float64(idxl)
        D[idxl]
    end
    @test d[idxl] == rv
end

@test fill(1., 10, 10, 10) == SharedArrays.shmem_fill(1.0, (10,10,10))
@test zeros(Int32, 10, 10, 10) == SharedArrays.shmem_fill(0, (10,10,10))

d = SharedArrays.shmem_rand(dims)
s = SharedArrays.shmem_rand(dims)
copyto!(s, d)
@test s == d
s = SharedArrays.shmem_rand(dims)
copyto!(s, sdata(d))
@test s == d
a = rand(Float64, dims)
@test sdata(a) == a

d = SharedArray{Int}(dims, init = D->fill!(D.loc_subarr_1d, myid()))
for p in procs(d)
    idxes_in_p = remotecall_fetch(p, d) do D
        parentindices(D.loc_subarr_1d)[1]
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
S = SharedArray{Int,2}(fn2, sz, init=D->D[localindices(D)] = myid())
@test S == filedata
filedata2 = similar(Atrue)
read!(fn2, filedata2)
@test filedata == filedata2
finalize(S)

# Appending to a file
fn3 = tempname()
write(fn3, fill(0x1, 4))
S = SharedArray{UInt8}(fn3, sz, 4, mode="a+", init=D->D[localindices(D)]=0x02)
len = prod(sz)+4
@test filesize(fn3) == len
filedata = Vector{UInt8}(undef, len)
read!(fn3, filedata)
@test all(filedata[1:4] .== 0x01)
@test all(filedata[5:end] .== 0x02)
finalize(S)

# call gc 3 times to avoid unlink: operation not permitted (EPERM) on Windows
S = nothing
@everywhere GC.gc()
@everywhere GC.gc()
@everywhere GC.gc()
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

d = SharedArrays.shmem_fill(1.0, (10,10,10))
@test fill(1., 100, 10) == reshape(d,(100,10))
d = SharedArrays.shmem_fill(1.0, (10,10,10))
@test_throws DimensionMismatch reshape(d,(50,))

# rand, randn
d = SharedArrays.shmem_rand(dims)
@test size(rand!(d)) == dims
d = SharedArrays.shmem_fill(1.0, dims)
@test size(randn!(d)) == dims

# similar
d = SharedArrays.shmem_rand(dims)
@test size(similar(d, ComplexF64)) == dims
@test size(similar(d, dims)) == dims

# issue #6362
d = SharedArrays.shmem_rand(dims)
s = copy(sdata(d))
ds = deepcopy(d)
@test ds == d
pids_d = procs(d)
remotecall_fetch(setindex!, pids_d[findfirst(id->(id != myid()), pids_d)::Int], d, 1.0, 1:10)
@test ds != d
@test s != d
copyto!(d, s)
@everywhere setid!(A) = A[localindices(A)] = myid()
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
B = @inferred(convert(SharedArray, copy(AA')))
@test B*A == AA'*AA

d=SharedArray{Int64,2}((10,10); init = D->fill!(D.loc_subarr_1d, myid()), pids=[id_me, id_other])
d2 = map(x->1, d)
@test reduce(+, d2) == 100

@test reduce(+, d) == ((50*id_me) + (50*id_other))
map!(x->1, d, d)
@test reduce(+, d) == 100

@test fill!(d, 1) == fill(1., 10, 10)
@test fill!(d, 2.) == fill(2, 10, 10)
@test d[:] == fill(2, 100)
@test d[:,1] == fill(2, 10)
@test d[1,:] == fill(2, 10)

# Boundary cases where length(S) <= length(pids)
@test 2.0 == remotecall_fetch(D->D[2], id_other, SharedArrays.shmem_fill(2.0, 2; pids=[id_me, id_other]))
@test 3.0 == remotecall_fetch(D->D[1], id_other, SharedArrays.shmem_fill(3.0, 1; pids=[id_me, id_other]))

# Shared arrays of singleton immutables
@everywhere struct ShmemFoo end
for T in [Nothing, ShmemFoo]
    local s = @inferred(SharedArray{T}(10))
    @test T() === remotecall_fetch(x->x[3], workers()[1], s)
end

# Issue #14664
d = SharedArray{Int}(10)
@sync @distributed for i=1:10
    d[i] = i
end

for (x,i) in enumerate(d)
    @test x == i
end

# complex
sd = SharedArray{Int}(10)
se = SharedArray{Int}(10)
@sync @distributed for i=1:10
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
    @test objectid(aorig) == objectid(a1)
    id = a1.id
    aorig = nothing
    a1 = remotecall_fetch(fill!, id_other, a1, 1.0)
    GC.gc(); GC.gc()
    a1 = remotecall_fetch(fill!, id_other, a1, 1.0)
    @test haskey(SharedArrays.sa_refs, id)
    finalize(a1)
    @test !haskey(SharedArrays.sa_refs, id)
end

#14399
let s = convert(SharedArray, [1,2,3,4])
    @test pmap(i->length(s), 1:2) == [4,4]
end
