# This file is a part of Julia. License is MIT: https://julialang.org/license

isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays
using DelimitedFiles
using Random
using LinearAlgebra
using Statistics

const OAs_name = join(fullname(OffsetArrays), ".")

if !isdefined(@__MODULE__, :T24Linear)
    include("testhelpers/arrayindexingtypes.jl")
end

let
# Basics
v0 = rand(4)
v = OffsetArray(v0, (-3,))
h = OffsetArray([-1,1,-2,2,0], (-3,))
@test axes(v) == (-2:1,)
@test size(v) == (4,)
@test size(v, 1) == 4
@test_throws DimensionMismatch Array(v)

A0 = [1 3; 2 4]
A = OffsetArray(A0, (-1,2))                   # IndexLinear
S = OffsetArray(view(A0, 1:2, 1:2), (-1,2))   # IndexCartesian
@test axes(A) == axes(S) == (0:1, 3:4)
@test size(A) == (2,2)
@test size(A, 1) == 2

# Scalar indexing
@test A[0,3] == A[1] == A[0,3,1] == S[0,3] == S[1] == S[0,3,1] == 1
@test A[1,3] == A[2] == A[1,3,1] == S[1,3] == S[2] == S[1,3,1] == 2
@test A[0,4] == A[3] == A[0,4,1] == S[0,4] == S[3] == S[0,4,1] == 3
@test A[1,4] == A[4] == A[1,4,1] == S[1,4] == S[4] == S[1,4,1] == 4
@test_throws BoundsError A[1,1]
@test_throws BoundsError S[1,1]
@test_throws BoundsError A[0,3,2]
@test_throws BoundsError S[0,3,2]
# partial indexing
S3 = OffsetArray(view(reshape(Vector(1:4*3*1), 4, 3, 1), 1:3, 1:2, :), (-1,-2,1))
@test S3[1,-1] == 2
@test S3[1,0] == 6
@test_throws BoundsError S3[1,1]
@test_throws BoundsError S3[1,-2]
S4 = OffsetArray(view(reshape(Vector(1:4*3*2), 4, 3, 2), 1:3, 1:2, :), (-1,-2,1))
@test S4[1,-1,2] == 2
@test S4[1,0,2] == 6
@test_throws BoundsError S4[1,1,2]
@test_throws BoundsError S4[1,-2,2]


# Vector indexing
@test A[:, 3] == S[:, 3] == OffsetArray([1,2], (A.offsets[1],))
@test A[:, 4] == S[:, 4] == OffsetArray([3,4], (A.offsets[1],))
@test_throws BoundsError A[:, 1]
@test_throws BoundsError S[:, 1]
@test A[0, :] == S[0, :] == OffsetArray([1,3], (A.offsets[2],))
@test A[1, :] == S[1, :] == OffsetArray([2,4], (A.offsets[2],))
@test_throws BoundsError A[2, :]
@test_throws BoundsError S[2, :]
@test A[0:1, 3] == S[0:1, 3] == [1,2]
@test A[[1,0], 3] == S[[1,0], 3] == [2,1]
@test A[0, 3:4] == S[0, 3:4] == [1,3]
@test A[1, [4,3]] == S[1, [4,3]] == [4,2]
@test A[:, :] == S[:, :] == A

A_3_3 = OffsetArray(Matrix{Int}(undef, 3,3), (-2,-1))
A_3_3[:, :] = reshape(1:9, 3, 3)
for i = 1:9 @test A_3_3[i] == i end
A_3_3[-1:1, 0:2] = reshape(1:9, 3, 3)
for i = 1:9 @test A_3_3[i] == i end
A_3_3[:, :] = 1:9
for i = 1:9 @test A_3_3[i] == i end
A_3_3[-1:1, 0:2] = 1:9
for i = 1:9 @test A_3_3[i] == i end
A_3_3[:] = 1:9
for i = 1:9 @test A_3_3[i] == i end
A_3_3[1:9] = 1:9
for i = 1:9 @test A_3_3[i] == i end

# CartesianIndexing
@test A[CartesianIndex((0,3))] == S[CartesianIndex((0,3))] == 1
@test_throws BoundsError A[CartesianIndex(1,1)]
@test_throws BoundsError S[CartesianIndex(1,1)]
@test eachindex(A) == 1:4
@test eachindex(S) == CartesianIndices(axes(S)) == CartesianIndices(map(Base.IdentityUnitRange, (0:1,3:4)))

# LinearIndices
# issue 27986
let a1 = [11,12,13], a2 = [1 2; 3 4]
    b1 = OffsetArray(a1, (-3,))
    i1 = LinearIndices(b1)
    @test i1[-2] == -2
    @test_throws BoundsError i1[-3]
    @test_throws BoundsError i1[1]
    @test i1[-2:end] === -2:0
    @test @inferred(i1[-2:0]) === -2:0
    @test_throws BoundsError i1[-3:end]
    @test_throws BoundsError i1[-2:1]
    b2 = OffsetArray(a2, (-3,5))
    i2 = LinearIndices(b2)
    @test i2[3] == 3
    @test_throws BoundsError i2[0]
    @test_throws BoundsError i2[5]
    @test @inferred(i2[2:3])   === 2:3
    @test @inferred(i2[1:2:4]) === 1:2:3
    @test_throws BoundsError i2[1:5]
    @test_throws BoundsError i2[1:2:5]
end

# logical indexing
@test A[A .> 2] == [3,4]
@test_throws BoundsError h[trues(2)]
@test_throws BoundsError h[trues(5)]
@test h[OffsetArray(trues(5), (-3,))] == parent(h)
@test h[OffsetArray([true,false,false,true,true], (-3,))] == parent(h)[[1,4,5]]
@test A[OffsetArray([true false; false true], A.offsets)] == [1,4]
@test A[OffsetArray([true true;  false true], A.offsets)] == [1,3,4]
@test_throws BoundsError A[[true true;  false true]]

# begin, end
a0 = rand(2,3,4,2)
a = OffsetArray(a0, (-2,-3,4,5))
@test a[begin,end,end,begin] == a0[begin,end,end,begin] ==
      a0[1,3,4,1] == a0[end-1,begin+2,begin+3,end-1]

# view
S = view(A, :, 3)
@test S == OffsetArray([1,2], (A.offsets[1],))
@test S[0] == 1
@test S[1] == 2
@test_throws BoundsError S[2]
@test axes(S) === (Base.IdentityUnitRange(0:1),)
S = view(A, 0, :)
@test S == OffsetArray([1,3], (A.offsets[2],))
@test S[3] == 1
@test S[4] == 3
@test_throws BoundsError S[1]
@test axes(S) === (Base.IdentityUnitRange(3:4),)
S = view(A, 0:0, 4)
@test S == [3]
@test S[1] == 3
@test_throws BoundsError S[0]
@test axes(S) === (Base.OneTo(1),)
S = view(A, 1, 3:4)
@test S == [2,4]
@test S[1] == 2
@test S[2] == 4
@test_throws BoundsError S[3]
@test axes(S) === (Base.OneTo(2),)
S = view(A, :, :)
@test S == A
@test S[0,3] == S[1] == 1
@test S[1,3] == S[2] == 2
@test S[0,4] == S[3] == 3
@test S[1,4] == S[4] == 4
@test_throws BoundsError S[1,1]
@test axes(S) === Base.IdentityUnitRange.((0:1, 3:4))
# https://github.com/JuliaArrays/OffsetArrays.jl/issues/27
g = OffsetArray(Vector(-2:3), (-3,))
gv = view(g, -1:2)
@test axes(gv, 1) === Base.OneTo(4)
@test collect(gv) == -1:2
gv = view(g, OffsetArray(-1:2, (-2,)))
@test axes(gv, 1) === Base.IdentityUnitRange(-1:2)
@test collect(gv) == -1:2
gv = view(g, OffsetArray(-1:2, (-1,)))
@test axes(gv, 1) === Base.IdentityUnitRange(0:3)
@test collect(gv) == -1:2

# iteration
for (a,d) in zip(A, A0)
    @test a == d
end


# show
io = IOBuffer()
show(io, v)
str = String(take!(io))
show(io, v0)
@test str == String(take!(io))
show(io, A)
str = String(take!(io))
@test str == "[1 3; 2 4]"
show(io, S)
str = String(take!(io))
@test str == "[1 3; 2 4]"
show(io, MIME("text/plain"), A)
strs = split(strip(String(take!(io))), '\n')
@test strs[2] == " 1  3"
@test strs[3] == " 2  4"
v = OffsetArray(rand(3), (-2,))
show(io, v)
str = String(take!(io))
show(io, parent(v))
@test str == String(take!(io))
smry = summary(v)
@test occursin("OffsetArray{Float64,1", smry)
@test occursin("with indices -1:1", smry)
function cmp_showf(printfunc, io, A; options = ())
    ioc = IOContext(io, :limit => true, :compact => true, options...)
    printfunc(ioc, A)
    str1 = String(take!(io))
    printfunc(ioc, parent(A))
    str2 = String(take!(io))
    @test str1 == str2
end
cmp_showf(Base.print_matrix, io, OffsetArray(rand(5,5), (10,-9)))       # rows&cols fit
cmp_showf(Base.print_matrix, io, OffsetArray(rand(10^3,5), (10,-9)))    # columns fit
cmp_showf(Base.print_matrix, io, OffsetArray(rand(5,10^3), (10,-9)))    # rows fit
cmp_showf(Base.print_matrix, io, OffsetArray(rand(10^3,10^3), (10,-9))) # neither fits
cmp_showf(Base.print_matrix, io, OffsetArray(reshape(range(-0.212121212121, stop=2/11, length=3*29), 3, 29), (-2, -15)); options=(:displaysize=>(53,210),))
cmp_showf(show, io, OffsetArray(collect(1:100), (100,)))   # issue #31641

targets1 = ["0-dimensional $OAs_name.OffsetArray{Float64,0,Array{Float64,0}}:\n1.0",
            "1-element $OAs_name.OffsetArray{Float64,1,Array{Float64,1}} with indices 2:2:\n 1.0",
            "1×1 $OAs_name.OffsetArray{Float64,2,Array{Float64,2}} with indices 2:2×3:3:\n 1.0",
            "1×1×1 $OAs_name.OffsetArray{Float64,3,Array{Float64,3}} with indices 2:2×3:3×4:4:\n[:, :, 4] =\n 1.0",
            "1×1×1×1 $OAs_name.OffsetArray{Float64,4,Array{Float64,4}} with indices 2:2×3:3×4:4×5:5:\n[:, :, 4, 5] =\n 1.0"]
targets2 = ["(fill(1.0), fill(1.0))",
            "([1.0], [1.0])",
            "([1.0], [1.0])",
            "([1.0], [1.0])",
            "([1.0], [1.0])"]
@testset "printing of OffsetArray with n=$n" for n = 0:4
    a = OffsetArray(fill(1.,ntuple(d->1,n)), ntuple(identity,n))
    show(IOContext(io, :limit => true), MIME("text/plain"), a)
    @test String(take!(io)) == targets1[n+1]
    show(IOContext(io, :limit => true), MIME("text/plain"), (a,a))
    @test String(take!(io)) == targets2[n+1]
end
P = OffsetArray(rand(8,8), (1,1))
PV = view(P, 2:3, :)
@test endswith(summary(PV), "with indices Base.OneTo(2)×2:9")

# Similar
B = similar(A, Float32)
@test isa(B, OffsetArray{Float32,2})
@test axes(B) === axes(A)
B = similar(A, (3,4))
@test isa(B, Array{Int,2})
@test size(B) == (3,4)
@test axes(B) === (Base.OneTo(3), Base.OneTo(4))
B = similar(A, (-3:3,1:4))
@test isa(B, OffsetArray{Int,2})
@test axes(B) === Base.IdentityUnitRange.((-3:3, 1:4))
B = similar(parent(A), (-3:3,1:4))
@test isa(B, OffsetArray{Int,2})
@test axes(B) === Base.IdentityUnitRange.((-3:3, 1:4))

# Indexing with OffsetArray indices
i1 = OffsetArray([2,1], (-5,))
i1 = OffsetArray([2,1], -5)
b = A0[i1, 1]
@test axes(b) === (Base.IdentityUnitRange(-4:-3),)
@test b[-4] == 2
@test b[-3] == 1
b = A0[1,i1]
@test axes(b) === (Base.IdentityUnitRange(-4:-3),)
@test b[-4] == 3
@test b[-3] == 1
v = view(A0, i1, 1)
@test axes(v) === (Base.IdentityUnitRange(-4:-3),)
v = view(A0, 1:1, i1)
@test axes(v) === (Base.OneTo(1), Base.IdentityUnitRange(-4:-3))

# copyto! and fill!
a = OffsetArray{Int}(undef, (-3:-1,))
fill!(a, -1)
copyto!(a, (1,2))   # non-array iterables
@test a[-3] == 1
@test a[-2] == 2
@test a[-1] == -1
fill!(a, -1)
copyto!(a, -2, (1,2))
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2
@test_throws BoundsError copyto!(a, 1, (1,2))
fill!(a, -1)
copyto!(a, -2, (1,2,3), 2)
@test a[-3] == -1
@test a[-2] == 2
@test a[-1] == 3
@test_throws BoundsError copyto!(a, -2, (1,2,3), 1)
fill!(a, -1)
copyto!(a, -2, (1,2,3), 1, 2)
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2

b = 1:2    # copy between AbstractArrays
bo = OffsetArray(1:2, (-3,))
copyto!(a, b)    # no BoundsError, see #34049
@test a[-3] == 1
@test a[-2] == 2
@test a[-1] == 2
fill!(a, -1)
copyto!(a, bo)
@test a[-3] == 1
@test a[-2] == 2
@test a[-1] == -1
fill!(a, -1)
copyto!(a, -2, bo)
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2
@test_throws BoundsError copyto!(a, -4, bo)
@test_throws BoundsError copyto!(a, -1, bo)
fill!(a, -1)
copyto!(a, -3, b, 2)
@test a[-3] == 2
@test a[-2] == a[-1] == -1
@test_throws BoundsError copyto!(a, -3, b, 1, 4)
am = OffsetArray{Int}(undef, (1:1, 7:9))  # for testing linear indexing
fill!(am, -1)
copyto!(am, b)
@test am[1] == 1
@test am[2] == 2
@test am[3] == -1
@test am[1,7] == 1
@test am[1,8] == 2
@test am[1,9] == -1

# map
dest = similar(am)
map!(+, dest, am, am)
@test dest[1,7] == 2
@test dest[1,8] == 4
@test dest[1,9] == -2

am = map(identity, a)
@test isa(am, OffsetArray)
@test am == a

# https://github.com/JuliaArrays/OffsetArrays.jl/issues/106
@test isequal(map(!, OffsetArray([true,missing],2)), OffsetArray([false, missing], 2))
@test isequal(map(!, OffsetArray([true missing; false true], 2, -1)), OffsetArray([false missing; true false], 2, -1))
P = view([true missing; false true; true false], 1:2:3, :)
@test IndexStyle(P) === IndexCartesian()
@test isequal(map(!, OffsetArray(P, 2, -1)), OffsetArray(map(!, P), 2, -1))
P = TSlow([true missing; false true])
@test IndexStyle(P) === IndexCartesian()
@test isequal(map(!, OffsetArray(P, 2, -1)), OffsetArray(map(!, P), 2, -1))

# dropdims
a0 = rand(1,1,8,8,1)
a = OffsetArray(a0, (-1,2,3,4,5))
@test @inferred(dropdims(a, dims=1)) == @inferred(dropdims(a, dims=(1,))) == OffsetArray(reshape(a, (1,8,8,1)), (2,3,4,5))
@test @inferred(dropdims(a, dims=5)) == @inferred(dropdims(a, dims=(5,))) == OffsetArray(reshape(a, (1,1,8,8)), (-1,2,3,4))
@test @inferred(dropdims(a, dims=(1,5))) == dropdims(a, dims=(5,1)) == OffsetArray(reshape(a, (1,8,8)), (2,3,4))
@test @inferred(dropdims(a, dims=(1,2,5))) == dropdims(a, dims=(5,2,1)) == OffsetArray(reshape(a, (8,8)), (3,4))
@test_throws ArgumentError dropdims(a, dims=0)
@test_throws ArgumentError dropdims(a, dims=(1,1))
@test_throws ArgumentError dropdims(a, dims=(1,2,1))
@test_throws ArgumentError dropdims(a, dims=(1,1,2))
@test_throws ArgumentError dropdims(a, dims=3)
@test_throws ArgumentError dropdims(a, dims=4)
@test_throws ArgumentError dropdims(a, dims=6)

# push!
v = OffsetArray(rand(4), (-3,))
v2 = copy(v)
@test push!(v2, 1) === v2
@test v2[axes(v, 1)] == v
@test v2[end] == 1
@test v2[begin] == v[begin] == v[-2]
v2 = copy(v)
@test push!(v2, 2, 1) === v2
@test v2[axes(v, 1)] == v
@test v2[end-1] == 2
@test v2[end] == 1

# append! from array
v2 = copy(v)
@test append!(v2, [2, 1]) === v2
@test v2[axes(v, 1)] == v
@test v2[lastindex(v)+1:end] == [2, 1]
# append! from HasLength iterator
v2 = copy(v)
@test append!(v2, (v for v in [2, 1])) === v2
@test v2[axes(v, 1)] == v
@test v2[lastindex(v)+1:end] == [2, 1]
# append! from SizeUnknown iterator
v2 = copy(v)
@test append!(v2, (v for v in [2, 1] if true)) === v2
@test v2[axes(v, 1)] == v
@test v2[lastindex(v)+1:end] == [2, 1]

# other functions
v = OffsetArray(v0, (-3,))
@test lastindex(v) == 1
@test v ≈ v
@test axes(v') === (Base.OneTo(1),Base.IdentityUnitRange(-2:1))
@test parent(v) == collect(v)
rv = reverse(v)
@test axes(rv) == axes(v)
@test rv[1] == v[-2]
@test rv[0] == v[-1]
@test rv[-1] == v[0]
@test rv[-2] == v[1]
cv = copy(v)
@test reverse!(cv) == rv

A = OffsetArray(rand(4,4), (-3,5))
@test lastindex(A) == 16
@test lastindex(A, 1) == 1
@test lastindex(A, 2) == 9
@test A ≈ A
@test axes(A') === Base.IdentityUnitRange.((6:9, -2:1))
@test parent(copy(A')) == copy(parent(A)')
@test collect(A) == parent(A)
@test maximum(A) == maximum(parent(A))
@test minimum(A) == minimum(parent(A))
@test extrema(A) == extrema(parent(A))
@test maximum(A, dims=1) == OffsetArray(maximum(parent(A), dims=1), A.offsets)
@test maximum(A, dims=2) == OffsetArray(maximum(parent(A), dims=2), A.offsets)
@test maximum(A, dims=1:2) == OffsetArray(maximum(parent(A), dims=1:2), A.offsets)
C = similar(A)
cumsum!(C, A, dims=1)
@test parent(C) == cumsum(parent(A), dims=1)
@test parent(cumsum(A, dims=1)) == cumsum(parent(A), dims=1)
cumsum!(C, A, dims=2)
@test parent(C) == cumsum(parent(A), dims=2)
R = similar(A, (1:1, 6:9))
maximum!(R, A)
@test parent(R) == maximum(parent(A), dims=1)
R = similar(A, (-2:1, 1:1))
maximum!(R, A)
@test parent(R) == maximum(parent(A), dims=2)
amin, iamin = findmin(A)
pmin, ipmin = findmin(parent(A))
@test amin == pmin
@test A[iamin] == amin
@test amin == parent(A)[ipmin]
amax, iamax = findmax(A)
pmax, ipmax = findmax(parent(A))
@test amax == pmax
@test A[iamax] == amax
@test amax == parent(A)[ipmax]
z = OffsetArray([0 0; 2 0; 0 0; 0 0], (-3,-1))
I = findall(!iszero, z)
@test I == [CartesianIndex(-1, 0)]
@test findall(!iszero,h) == [-2:1;]
@test findall(x->x>0, h) == [-1,1]
@test findall(x->x<0, h) == [-2,0]
@test findall(x->x==0, h) == [2]
@test mean(A_3_3) == median(A_3_3) == 5
@test mean(x->2x, A_3_3) == 10
@test mean(A_3_3, dims=1) == median(A_3_3, dims=1) == OffsetArray([2 5 8], A_3_3.offsets)
@test mean(A_3_3, dims=2) == median(A_3_3, dims=2) == OffsetArray(reshape([4,5,6],(3,1)), A_3_3.offsets)
@test var(A_3_3) == 7.5
@test std(A_3_3, dims=1) == OffsetArray([1 1 1], A_3_3.offsets)
@test std(A_3_3, dims=2) == OffsetArray(reshape([3,3,3], (3,1)), A_3_3.offsets)
@test sum(OffsetArray(fill(1,3000), -1000)) == 3000

# https://github.com/JuliaArrays/OffsetArrays.jl/issues/92
A92 = OffsetArray(reshape(1:27, 3, 3, 3), -2, -2, -2)
B92 = view(A92, :, :, -1:0)
@test axes(B92) == (-1:1, -1:1, 1:2)
@test sum(B92, dims=(2,3)) == OffsetArray(reshape([51,57,63], Val(3)), -2, -2, 0)
B92 = view(A92, :, :, Base.IdentityUnitRange(-1:0))
@test sum(B92, dims=(2,3)) == OffsetArray(reshape([51,57,63], Val(3)), -2, -2, -2)

@test norm(v) ≈ norm(parent(v))
@test norm(A) ≈ norm(parent(A))
@test dot(v, v) ≈ dot(v0, v0)

# Prior to its removal from Base, cumsum_kbn was used here. To achieve the same level of
# accuracy in the tests, we need to use BigFloats with enlarged precision.
@testset "high-precision array reduction" begin
    setprecision(BigFloat, 500) do
        v  = OffsetArray(BigFloat[1,1e100,1,-1e100], (-3,)) .* 1000
        v2 = OffsetArray(BigFloat[1,-1e100,1,1e100], ( 5,)) .* 1000
        @test isa(v, OffsetArray)
        cv  = OffsetArray(BigFloat[1, 1e100, 1e100,2], (-3,)) .* 1000
        cv2 = OffsetArray(BigFloat[1,-1e100,-1e100,2], ( 5,)) .* 1000
        @test cumsum(v) ≈ cv
        @test cumsum(v2) ≈ cv2
        @test sum(v) ≈ sum(parent(v))
    end
end

io = IOBuffer()
writedlm(io, A)
seek(io, 0)
@test readdlm(io, eltype(A)) == parent(A)

amin, amax = extrema(parent(A))
@test clamp.(A, (amax+amin)/2, amax).parent == clamp.(parent(A), (amax+amin)/2, amax)

@test unique(A, dims=1) == OffsetArray(parent(A), 0, first(axes(A, 2)) - 1)
@test unique(A, dims=2) == OffsetArray(parent(A), first(axes(A, 1)) - 1, 0)
v = OffsetArray(rand(8), (-2,))
@test sort(v) == OffsetArray(sort(parent(v)), v.offsets)
@test sortslices(A, dims=1) == OffsetArray(sortslices(parent(A), dims=1), A.offsets)
@test sortslices(A, dims=2) == OffsetArray(sortslices(parent(A), dims=2), A.offsets)
@test sort(A, dims=1) == OffsetArray(sort(parent(A), dims=1), A.offsets)
@test sort(A, dims=2) == OffsetArray(sort(parent(A), dims=2), A.offsets)
# Issue #33977
soa = OffsetArray([2,2,3], -2)
@test searchsorted(soa, 1) === -1:-2
@test searchsortedfirst(soa, 1) == -1
@test searchsortedlast(soa, 1) == -2
@test first(sort!(soa; alg=QuickSort)) == 2
@test first(sort!(soa; alg=MergeSort)) == 2
soa = OffsetArray([2,2,3], typemax(Int)-4)
@test searchsorted(soa, 1) == typemax(Int)-3:typemax(Int)-4
@test searchsortedfirst(soa, 2) == typemax(Int) - 3
@test searchsortedlast(soa, 2) == typemax(Int) - 2
@test first(sort!(soa; alg=QuickSort)) == 2
@test first(sort!(soa; alg=MergeSort)) == 2

@test mapslices(sort, A, dims=1) == OffsetArray(mapslices(sort, parent(A), dims=1), A.offsets)
@test mapslices(sort, A, dims=2) == OffsetArray(mapslices(sort, parent(A), dims=2), A.offsets)

@test rotl90(A) == OffsetArray(rotl90(parent(A)), A.offsets[[2,1]])
@test rotr90(A) == OffsetArray(rotr90(parent(A)), A.offsets[[2,1]])
@test reverse(A, dims=1) == OffsetArray(reverse(parent(A), dims=1), A.offsets)
@test reverse(A, dims=2) == OffsetArray(reverse(parent(A), dims=2), A.offsets)

@test A .+ 1 == OffsetArray(parent(A) .+ 1, A.offsets)
@test 2*A == OffsetArray(2*parent(A), A.offsets)
@test A+A == OffsetArray(parent(A)+parent(A), A.offsets)
@test A.*A == OffsetArray(parent(A).*parent(A), A.offsets)

@test circshift(A, (-1,2)) == OffsetArray(circshift(parent(A), (-1,2)), A.offsets)

src = reshape(Vector(1:16), (4,4))
dest = OffsetArray(Matrix{Int}(undef, 4,4), (-1,1))
circcopy!(dest, src)
@test parent(dest) == [8 12 16 4; 5 9 13 1; 6 10 14 2; 7 11 15 3]
@test dest[1:3,2:4] == src[1:3,2:4]

# reshape
A = OffsetArray(rand(4,4), (-3,5))
@test vec(A) == reshape(A, :) == reshape(A, 16) == reshape(A, Val(1)) == A[:] == vec(A.parent)
A = OffsetArray(view(rand(4,4), 1:4, 4:-1:1), (-3,5))
@test vec(A) == reshape(A, :) == reshape(A, 16) == reshape(A, Val(1)) == A[:] == vec(A.parent)
# issue #33614
A = OffsetArray(-1:0, (-2,))
@test reshape(A, :) === A
Arsc = reshape(A, :, 1)
Arss = reshape(A, 2, 1)
@test Arsc[1,1] == Arss[1,1] == -1
@test Arsc[2,1] == Arss[2,1] == 0
@test_throws BoundsError Arsc[0,1]
@test_throws BoundsError Arss[0,1]
A = OffsetArray([-1,0], (-2,))
Arsc = reshape(A, :, 1)
Arsc[1,1] = 5
@test first(A) == 5

# broadcast
a = [1]
b = OffsetArray(a, (0,))
@test @inferred(a .+ b) == [2]
a = OffsetArray([1, -2, 1], (-2,))
@test a .* a' == OffsetArray([ 1 -2  1;
                              -2  4 -2;
                               1 -2  1], (-2,-2))

end # let

# Check that similar throws a MethodError rather than a
# StackOverflowError if no appropriate method has been defined
# (#18107)
module SimilarUR
    using Test
    struct MyURange <: AbstractUnitRange{Int}
        start::Int
        stop::Int
    end
    ur = MyURange(1,3)
    a = Vector{Int}(undef, 2)
    @test_throws MethodError similar(a, ur)
    @test_throws MethodError similar(a, Float64, ur)
    @test_throws MethodError similar(a, Float64, (ur,))
    @test_throws MethodError similar(a, (2.0,3.0))
end

@testset "Issue 28101" begin
    A = OffsetArray(reshape(16:-1:1, (4, 4)), (-3,5))
    @test maximum(A, dims=1) == OffsetArray(maximum(parent(A), dims=1), A.offsets)
end

@testset "in-place reductions with mismatched dimensionalities" begin
    B = OffsetArray(reshape(1:24, 4, 3, 2), -5, 6, -7)
    for R in (fill(0, -4:-1), fill(0, -4:-1, 7:7), fill(0, -4:-1, 7:7, -6:-6))
        @test @inferred(maximum!(R, B)) == reshape(maximum(B, dims=(2,3)), axes(R)) == reshape(21:24, axes(R))
        @test @allocated(maximum!(R, B)) <= 800
        @test @inferred(minimum!(R, B)) == reshape(minimum(B, dims=(2,3)), axes(R)) == reshape(1:4, axes(R))
        @test @allocated(minimum!(R, B)) <= 800
    end
    for R in (fill(0, -4:-4, 7:9), fill(0, -4:-4, 7:9, -6:-6))
        @test @inferred(maximum!(R, B)) == reshape(maximum(B, dims=(1,3)), axes(R)) == reshape(16:4:24, axes(R))
        @test @allocated(maximum!(R, B)) <= 800
        @test @inferred(minimum!(R, B)) == reshape(minimum(B, dims=(1,3)), axes(R)) == reshape(1:4:9, axes(R))
        @test @allocated(minimum!(R, B)) <= 800
    end
    @test_throws DimensionMismatch maximum!(fill(0, -4:-1, 7:7, -6:-6, 1:1), B)
    @test_throws DimensionMismatch minimum!(fill(0, -4:-1, 7:7, -6:-6, 1:1), B)
    @test_throws DimensionMismatch maximum!(fill(0, -4:-4, 7:9, -6:-6, 1:1), B)
    @test_throws DimensionMismatch minimum!(fill(0, -4:-4, 7:9, -6:-6, 1:1), B)
    @test_throws DimensionMismatch maximum!(fill(0, -4:-4, 7:7, -6:-5, 1:1), B)
    @test_throws DimensionMismatch minimum!(fill(0, -4:-4, 7:7, -6:-5, 1:1), B)
end
