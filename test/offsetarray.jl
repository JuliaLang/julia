# This file is a part of Julia. License is MIT: http://julialang.org/license

isdefined(Main, :TestHelpers) || @eval Main include(joinpath(dirname(@__FILE__), "TestHelpers.jl"))
using TestHelpers.OAs

const OAs_name = join(fullname(OAs), ".")

let
# Basics
v0 = rand(4)
v = OffsetArray(v0, (-3,))
h = OffsetArray([-1,1,-2,2,0], (-3,))
@test indices(v) == (-2:1,)
@test_throws ErrorException size(v)
@test_throws ErrorException size(v, 1)

A0 = [1 3; 2 4]
A = OffsetArray(A0, (-1,2))                   # LinearFast
S = OffsetArray(view(A0, 1:2, 1:2), (-1,2))   # LinearSlow
@test indices(A) == indices(S) == (0:1, 3:4)
@test_throws ErrorException size(A)
@test_throws ErrorException size(A, 1)

# Scalar indexing
@test A[0,3] == A[1] == S[0,3] == S[1] == 1
@test A[1,3] == A[2] == S[1,3] == S[2] == 2
@test A[0,4] == A[3] == S[0,4] == S[3] == 3
@test A[1,4] == A[4] == S[1,4] == S[4] == 4
@test_throws BoundsError A[1,1]
@test_throws BoundsError S[1,1]

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

A_3_3 = OffsetArray(Array{Int}(3,3), (-2,-1))
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
@test eachindex(S) == CartesianRange((0:1,3:4))

# logical indexing
@test A[A .> 2] == [3,4]
@test_throws BoundsError h[trues(2)]
@test_throws BoundsError h[trues(5)]
@test h[OffsetArray(trues(5), (-3,))] == parent(h)
@test h[OffsetArray([true,false,false,true,true], (-3,))] == parent(h)[[1,4,5]]
@test A[OffsetArray([true false; false true], A.offsets)] == [1,4]
@test A[OffsetArray([true true;  false true], A.offsets)] == [1,3,4]
@test_throws BoundsError A[[true true;  false true]]

# view
S = view(A, :, 3)
@test S == OffsetArray([1,2], (A.offsets[1],))
@test S[0] == 1
@test S[1] == 2
@test_throws BoundsError S[2]
@test indices(S) === (0:1,)
S = view(A, 0, :)
@test S == OffsetArray([1,3], (A.offsets[2],))
@test S[3] == 1
@test S[4] == 3
@test_throws BoundsError S[1]
@test indices(S) === (3:4,)
S = view(A, 0:0, 4)
@test S == [3]
@test S[1] == 3
@test_throws BoundsError S[0]
@test indices(S) === (Base.OneTo(1),)
S = view(A, 1, 3:4)
@test S == [2,4]
@test S[1] == 2
@test S[2] == 4
@test_throws BoundsError S[3]
@test indices(S) === (Base.OneTo(2),)
S = view(A, :, :)
@test S == A
@test S[0,3] == S[1] == 1
@test S[1,3] == S[2] == 2
@test S[0,4] == S[3] == 3
@test S[1,4] == S[4] == 4
@test_throws BoundsError S[1,1]
@test indices(S) === (0:1, 3:4)

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
@test contains(smry, "OffsetArray{Float64,1")
@test contains(smry, "with indices -1:1")
function cmp_showf(printfunc, io, A)
    ioc = IOContext(io, limit=true, compact=true)
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
targets1 = ["0-dimensional $OAs_name.OffsetArray{Float64,0,Array{Float64,0}}:\n1.0",
            "$OAs_name.OffsetArray{Float64,1,Array{Float64,1}} with indices 2:2:\n 1.0",
            "$OAs_name.OffsetArray{Float64,2,Array{Float64,2}} with indices 2:2×3:3:\n 1.0",
            "$OAs_name.OffsetArray{Float64,3,Array{Float64,3}} with indices 2:2×3:3×4:4:\n[:, :, 4] =\n 1.0",
            "$OAs_name.OffsetArray{Float64,4,Array{Float64,4}} with indices 2:2×3:3×4:4×5:5:\n[:, :, 4, 5] =\n 1.0"]
targets2 = ["(1.0, 1.0)",
            "([1.0], [1.0])",
            "([1.0], [1.0])",
            "([1.0], [1.0])",
            "([1.0], [1.0])"]
for n = 0:4
    a = OffsetArray(ones(Float64,ntuple(d->1,n)), ntuple(identity,n))
    show(IOContext(io, limit=true), MIME("text/plain"), a)
    @test String(take!(io)) == targets1[n+1]
    show(IOContext(io, limit=true), MIME("text/plain"), (a,a))
    @test String(take!(io)) == targets2[n+1]
end
P = OffsetArray(rand(8,8), (1,1))
PV = view(P, 2:3, :)
@test endswith(summary(PV), "with indices Base.OneTo(2)×2:9")

# Similar
B = similar(A, Float32)
@test isa(B, OffsetArray{Float32,2})
@test indices(B) === indices(A)
B = similar(A, (3,4))
@test isa(B, Array{Int,2})
@test size(B) == (3,4)
@test indices(B) === (Base.OneTo(3), Base.OneTo(4))
B = similar(A, (-3:3,1:4))
@test isa(B, OffsetArray{Int,2})
@test indices(B) === (-3:3, 1:4)
B = similar(parent(A), (-3:3,1:4))
@test isa(B, OffsetArray{Int,2})
@test indices(B) === (-3:3, 1:4)

# Indexing with OffsetArray indices
i1 = OffsetArray([2,1], (-5,))
i1 = OffsetArray([2,1], -5)
b = A0[i1, 1]
@test indices(b) === (-4:-3,)
@test b[-4] == 2
@test b[-3] == 1
b = A0[1,i1]
@test indices(b) === (-4:-3,)
@test b[-4] == 3
@test b[-3] == 1
v = view(A0, i1, 1)
@test indices(v) === (-4:-3,)
v = view(A0, 1:1, i1)
@test indices(v) === (Base.OneTo(1), -4:-3)

# copy! and fill!
a = OffsetArray{Int}((-3:-1,))
fill!(a, -1)
copy!(a, (1,2))   # non-array iterables
@test a[-3] == 1
@test a[-2] == 2
@test a[-1] == -1
fill!(a, -1)
copy!(a, -2, (1,2))
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2
@test_throws BoundsError copy!(a, 1, (1,2))
fill!(a, -1)
copy!(a, -2, (1,2,3), 2)
@test a[-3] == -1
@test a[-2] == 2
@test a[-1] == 3
@test_throws BoundsError copy!(a, -2, (1,2,3), 1)
fill!(a, -1)
copy!(a, -2, (1,2,3), 1, 2)
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2

b = 1:2    # copy between AbstractArrays
bo = OffsetArray(1:2, (-3,))
@test_throws BoundsError copy!(a, b)
fill!(a, -1)
copy!(a, bo)
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2
fill!(a, -1)
copy!(a, -2, bo)
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2
@test_throws BoundsError copy!(a, -4, bo)
@test_throws BoundsError copy!(a, -1, bo)
fill!(a, -1)
copy!(a, -3, b, 2)
@test a[-3] == 2
@test a[-2] == a[-1] == -1
@test_throws BoundsError copy!(a, -3, b, 1, 4)
am = OffsetArray{Int}((1:1, 7:9))  # for testing linear indexing
fill!(am, -1)
copy!(am, b)
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

# other functions
v = OffsetArray(v0, (-3,))
@test v ≈ v
@test indices(v') === (Base.OneTo(1),-2:1)
A = OffsetArray(rand(4,4), (-3,5))
@test A ≈ A
@test indices(A') === (6:9, -2:1)
@test parent(A') == parent(A)'
@test maximum(A) == maximum(parent(A))
@test minimum(A) == minimum(parent(A))
@test extrema(A) == extrema(parent(A))
@test maximum(A, 1) == OffsetArray(maximum(parent(A), 1), (0,A.offsets[2]))
@test maximum(A, 2) == OffsetArray(maximum(parent(A), 2), (A.offsets[1],0))
@test maximum(A, 1:2) == maximum(parent(A), 1:2)
C = similar(A)
cumsum!(C, A, 1)
@test parent(C) == cumsum(parent(A), 1)
@test parent(cumsum(A, 1)) == cumsum(parent(A), 1)
cumsum!(C, A, 2)
@test parent(C) == cumsum(parent(A), 2)
R = similar(A, (1:1, 6:9))
maximum!(R, A)
@test parent(R) == maximum(parent(A), 1)
R = similar(A, (-2:1, 1:1))
maximum!(R, A)
@test parent(R) == maximum(parent(A), 2)
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
I,J = findn(z)
@test I == [-1]
@test J == [0]
I,J,N = findnz(z)
@test I == [-1]
@test J == [0]
@test N == [2]
@test find(h) == [-2:1;]
@test find(x->x>0, h) == [-1,1]
@test find(x->x<0, h) == [-2,0]
@test find(x->x==0, h) == [2]
@test mean(A_3_3) == median(A_3_3) == 5
@test mean(x->2x, A_3_3) == 10
@test mean(A_3_3, 1) == median(A_3_3, 1) == OffsetArray([2 5 8], (0,A_3_3.offsets[2]))
@test mean(A_3_3, 2) == median(A_3_3, 2) == OffsetArray(reshape([4,5,6],(3,1)), (A_3_3.offsets[1],0))
@test var(A_3_3) == 7.5
@test std(A_3_3, 1) == OffsetArray([1 1 1], (0,A_3_3.offsets[2]))
@test std(A_3_3, 2) == OffsetArray(reshape([3,3,3], (3,1)), (A_3_3.offsets[1],0))
@test sum(OffsetArray(ones(Int,3000), -1000)) == 3000

@test vecnorm(v) ≈ vecnorm(parent(v))
@test vecnorm(A) ≈ vecnorm(parent(A))
@test vecdot(v, v) ≈ vecdot(v0, v0)

v  = OffsetArray([1,1e100,1,-1e100], (-3,))*1000
v2 = OffsetArray([1,-1e100,1,1e100], (5,))*1000
@test isa(v, OffsetArray)
cv  = OffsetArray([1,1e100,1e100,2], (-3,))*1000
cv2 = OffsetArray([1,-1e100,-1e100,2], (5,))*1000
@test isequal(cumsum_kbn(v), cv)
@test isequal(cumsum_kbn(v2), cv2)
@test isequal(sum_kbn(v), sum_kbn(parent(v)))

io = IOBuffer()
writedlm(io, A)
seek(io, 0)
@test readdlm(io, eltype(A)) == parent(A)

amin, amax = extrema(parent(A))
@test clamp.(A, (amax+amin)/2, amax).parent == clamp.(parent(A), (amax+amin)/2, amax)

@test unique(A, 1) == parent(A)
@test unique(A, 2) == parent(A)
v = OffsetArray(rand(8), (-2,))
@test sort(v) == OffsetArray(sort(parent(v)), v.offsets)
@test sortrows(A) == OffsetArray(sortrows(parent(A)), A.offsets)
@test sortcols(A) == OffsetArray(sortcols(parent(A)), A.offsets)
@test sort(A, 1) == OffsetArray(sort(parent(A), 1), A.offsets)
@test sort(A, 2) == OffsetArray(sort(parent(A), 2), A.offsets)

@test mapslices(sort, A, 1) == OffsetArray(mapslices(sort, parent(A), 1), A.offsets)
@test mapslices(sort, A, 2) == OffsetArray(mapslices(sort, parent(A), 2), A.offsets)

@test rotl90(A) == OffsetArray(rotl90(parent(A)), A.offsets[[2,1]])
@test rotr90(A) == OffsetArray(rotr90(parent(A)), A.offsets[[2,1]])
@test flipdim(A, 1) == OffsetArray(flipdim(parent(A), 1), A.offsets)
@test flipdim(A, 2) == OffsetArray(flipdim(parent(A), 2), A.offsets)

@test A+1 == OffsetArray(parent(A)+1, A.offsets)
@test 2*A == OffsetArray(2*parent(A), A.offsets)
@test A+A == OffsetArray(parent(A)+parent(A), A.offsets)
@test A.*A == OffsetArray(parent(A).*parent(A), A.offsets)

@test circshift(A, (-1,2)) == OffsetArray(circshift(parent(A), (-1,2)), A.offsets)

src = reshape(collect(1:16), (4,4))
dest = OffsetArray(Array{Int}(4,4), (-1,1))
circcopy!(dest, src)
@test parent(dest) == [8 12 16 4; 5 9 13 1; 6 10 14 2; 7 11 15 3]
@test dest[1:3,2:4] == src[1:3,2:4]

e = eye(5)
a = [e[:,1], e[:,2], e[:,3], e[:,4], e[:,5]]
a1 = zeros(5)
c = [ones(Complex{Float64}, 5),
     exp.(-2*pi*im*(0:4)/5),
     exp.(-4*pi*im*(0:4)/5),
     exp.(-6*pi*im*(0:4)/5),
     exp.(-8*pi*im*(0:4)/5)]
for s = -5:5
    for i = 1:5
        thisa = OffsetArray(a[i], (s,))
        thisc = c[mod1(i+s+5,5)]
        @test fft(thisa) ≈ thisc
        @test fft(thisa, 1) ≈ thisc
        @test ifft(fft(thisa)) ≈ circcopy!(a1, thisa)
        @test ifft(fft(thisa, 1), 1) ≈ circcopy!(a1, thisa)
        @test rfft(thisa) ≈ thisc[1:3]
        @test rfft(thisa, 1) ≈ thisc[1:3]
        @test irfft(rfft(thisa, 1), 5, 1) ≈ a1
        @test irfft(rfft(thisa, 1), 5, 1) ≈ a1
    end
end

end # let

# Check that similar throws a MethodError rather than a
# StackOverflowError if no appropriate method has been defined
# (#18107)
module SimilarUR
    using Base.Test
    immutable MyURange <: AbstractUnitRange{Int}
        start::Int
        stop::Int
    end
    ur = MyURange(1,3)
    a = Array{Int}(2)
    @test_throws MethodError similar(a, ur)
    @test_throws MethodError similar(a, Float64, ur)
    @test_throws MethodError similar(a, Float64, (ur,))
    @test_throws MethodError similar(a, (2.0,3.0))
end
