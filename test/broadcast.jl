# This file is a part of Julia. License is MIT: http://julialang.org/license

module TestBroadcastInternals

using Base.Broadcast: broadcast_shape, check_broadcast_shape, newindex, _bcs, _bcsm
using Base: Test, OneTo

@test @inferred(_bcs((), (3,5), (3,5))) == (3,5)
@test @inferred(_bcs((), (3,1), (3,5))) == (3,5)
@test @inferred(_bcs((), (3,),  (3,5))) == (3,5)
@test @inferred(_bcs((), (3,5), (3,)))  == (3,5)
@test_throws DimensionMismatch _bcs((), (3,5), (4,5))
@test_throws DimensionMismatch _bcs((), (3,5), (3,4))
@test @inferred(_bcs((), (-1:1, 2:5), (-1:1, 2:5))) == (-1:1, 2:5)
@test @inferred(_bcs((), (-1:1, 2:5), (1, 2:5)))    == (-1:1, 2:5)
@test @inferred(_bcs((), (-1:1, 1),   (1, 2:5)))    == (-1:1, 2:5)
@test @inferred(_bcs((), (-1:1,),     (-1:1, 2:5))) == (-1:1, 2:5)
@test_throws DimensionMismatch _bcs((), (-1:1, 2:6), (-1:1, 2:5))
@test_throws DimensionMismatch _bcs((), (-1:1, 2:5), (2, 2:5))

@test @inferred(broadcast_shape(zeros(3,4), zeros(3,4))) == (OneTo(3),OneTo(4))
@test @inferred(broadcast_shape(zeros(3,4), zeros(3)))   == (OneTo(3),OneTo(4))
@test @inferred(broadcast_shape(zeros(3),   zeros(3,4))) == (OneTo(3),OneTo(4))
@test @inferred(broadcast_shape(zeros(3), zeros(1,4), zeros(1))) == (OneTo(3),OneTo(4))

check_broadcast_shape((OneTo(3),OneTo(5)), zeros(3,5))
check_broadcast_shape((OneTo(3),OneTo(5)), zeros(3,1))
check_broadcast_shape((OneTo(3),OneTo(5)), zeros(3))
check_broadcast_shape((OneTo(3),OneTo(5)), zeros(3,5), zeros(3))
check_broadcast_shape((OneTo(3),OneTo(5)), zeros(3,5), 1)
check_broadcast_shape((OneTo(3),OneTo(5)), 5, 2)
@test_throws DimensionMismatch check_broadcast_shape((OneTo(3),OneTo(5)), zeros(2,5))
@test_throws DimensionMismatch check_broadcast_shape((OneTo(3),OneTo(5)), zeros(3,4))
@test_throws DimensionMismatch check_broadcast_shape((OneTo(3),OneTo(5)), zeros(3,4,2))
@test_throws DimensionMismatch check_broadcast_shape((OneTo(3),OneTo(5)), zeros(3,5), zeros(2))

check_broadcast_shape((-1:1, 6:9), (-1:1, 6:9))
check_broadcast_shape((-1:1, 6:9), (-1:1, 1))
check_broadcast_shape((-1:1, 6:9), (1, 6:9))
@test_throws DimensionMismatch check_broadcast_shape((-1:1, 6:9), (-1, 6:9))
@test_throws DimensionMismatch check_broadcast_shape((-1:1, 6:9), (-1:1, 6))
check_broadcast_shape((-1:1, 6:9), 1)

ci(x) = CartesianIndex(x)
@test @inferred(newindex(ci((2,2)), (true, true)))   == ci((2,2))
@test @inferred(newindex(ci((2,2)), (true, false)))  == ci((2,1))
@test @inferred(newindex(ci((2,2)), (false, true)))  == ci((1,2))
@test @inferred(newindex(ci((2,2)), (false, false))) == ci((1,1))
@test @inferred(newindex(ci((2,2)), (true,)))   == ci((2,))
@test @inferred(newindex(ci((2,2)), (false,))) == ci((1,))
@test @inferred(newindex(ci((2,2)), ())) == 1

end

function as_sub(x::AbstractVector)
    y = similar(x, eltype(x), tuple(([size(x)...]*2)...))
    y = view(y, 2:2:length(y))
    y[:] = x[:]
    y
end
function as_sub(x::AbstractMatrix)
    y = similar(x, eltype(x), tuple(([size(x)...]*2)...))
    y = view(y, 2:2:size(y,1), 2:2:size(y,2))
    for j=1:size(x,2)
        for i=1:size(x,1)
            y[i,j] = x[i,j]
        end
    end
    y
end
function as_sub{T}(x::AbstractArray{T,3})
    y = similar(x, eltype(x), tuple(([size(x)...]*2)...))
    y = view(y, 2:2:size(y,1), 2:2:size(y,2), 2:2:size(y,3))
    for k=1:size(x,3)
        for j=1:size(x,2)
            for i=1:size(x,1)
                y[i,j,k] = x[i,j,k]
            end
        end
    end
    y
end

bittest(f::Function, ewf::Function, a...) = (@test ewf(a...) == BitArray(broadcast(f, a...)))
n1 = 21
n2 = 32
n3 = 17
rb = 1:5

for arr in (identity, as_sub)
    @test broadcast(+, arr(eye(2)), arr([1, 4])) == [2 1; 4 5]
    @test broadcast(+, arr(eye(2)), arr([1  4])) == [2 4; 1 5]
    @test broadcast(+, arr([1  0]), arr([1, 4])) == [2 1; 5 4]
    @test broadcast(+, arr([1, 0]), arr([1  4])) == [2 5; 1 4]
    @test broadcast(+, arr([1, 0]), arr([1, 4])) == [2, 4]
    @test broadcast(+, arr([1, 0]), 2) == [3, 2]

    @test @inferred(arr(eye(2)) .+ arr([1, 4])) == arr([2 1; 4 5])
    @test arr(eye(2)) .+ arr([1  4]) == arr([2 4; 1 5])
    @test arr([1  0]) .+ arr([1, 4]) == arr([2 1; 5 4])
    @test arr([1, 0]) .+ arr([1  4]) == arr([2 5; 1 4])
    @test arr([1, 0]) .+ arr([1, 4]) == arr([2, 4])
    @test arr([1]) .+ arr([]) == arr([])

    A = arr(eye(2)); @test broadcast!(+, A, A, arr([1, 4])) == arr([2 1; 4 5])
    A = arr(eye(2)); @test broadcast!(+, A, A, arr([1  4])) == arr([2 4; 1 5])
    A = arr([1  0]); @test_throws DimensionMismatch broadcast!(+, A, A, arr([1, 4]))
    A = arr([1  0]); @test broadcast!(+, A, A, arr([1  4])) == arr([2 4])
    A = arr([1  0]); @test broadcast!(+, A, A, 2) == arr([3 2])

    @test arr([ 1    2])   .* arr([3,   4])   == [ 3 6; 4 8]
    @test arr([24.0 12.0]) ./ arr([2.0, 3.0]) == [12 6; 8 4]
    @test arr([1 2]) ./ arr([3, 4]) == [1/3 2/3; 1/4 2/4]
    @test arr([1 2]) .\ arr([3, 4]) == [3 1.5; 4 2]
    @test arr([3 4]) .^ arr([1, 2]) == [3 4; 9 16]
    @test arr(BitArray([true false])) .* arr(BitArray([true, true])) == [true false; true false]
    @test arr(BitArray([true false])) .^ arr(BitArray([false, true])) == [true true; true false]
    @test arr(BitArray([true false])) .^ arr([0, 3]) == [true true; true false]

    M = arr([11 12; 21 22])
    @test broadcast_getindex(M, eye(Int, 2).+1,arr([1, 2])) == [21 11; 12 22]
    @test_throws BoundsError broadcast_getindex(M, eye(Int, 2).+1,arr([1, -1]))
    @test_throws BoundsError broadcast_getindex(M, eye(Int, 2).+1,arr([1, 2]), [2])
    @test broadcast_getindex(M, eye(Int, 2).+1,arr([2, 1]), [1]) == [22 12; 11 21]

    A = arr(zeros(2,2))
    broadcast_setindex!(A, arr([21 11; 12 22]), eye(Int, 2).+1,arr([1, 2]))
    @test A == M
    broadcast_setindex!(A, 5, [1,2], [2 2])
    @test A == [11 5; 21 5]
    broadcast_setindex!(A, 7, [1,2], [1 2])
    @test A == fill(7, 2, 2)
    A = arr(zeros(3,3))
    broadcast_setindex!(A, 10:12, 1:3, 1:3)
    @test A == diagm(10:12)
    @test_throws BoundsError broadcast_setindex!(A, 7, [1,-1], [1 2])

    for (f, ewf) in (((==), (.==)),
                     ((<) , (.<) ),
                     ((!=), (.!=)),
                     ((<=), (.<=)))
        bittest(f, ewf, arr(eye(2)), arr([1, 4]))
        bittest(f, ewf, arr(eye(2)), arr([1  4]))
        bittest(f, ewf, arr([0, 1]), arr([1  4]))
        bittest(f, ewf, arr([0  1]), arr([1, 4]))
        bittest(f, ewf, arr([1, 0]), arr([1, 4]))
        bittest(f, ewf, arr(rand(rb, n1, n2, n3)), arr(rand(rb, n1, n2, n3)))
        bittest(f, ewf, arr(rand(rb,  1, n2, n3)), arr(rand(rb, n1,  1, n3)))
        bittest(f, ewf, arr(rand(rb,  1, n2,  1)), arr(rand(rb, n1,  1, n3)))
        bittest(f, ewf, arr(bitrand(n1, n2, n3)), arr(bitrand(n1, n2, n3)))
    end
end

r1 = 1:1
r2 = 1:5
ratio = [1,1/2,1/3,1/4,1/5]
@test r1.*r2 == [1:5;]
@test r1./r2 == ratio
m = [1:2;]'
@test m.*r2 == [1:5 2:2:10]
@test m./r2 ≈ [ratio 2ratio]
@test m./[r2;] ≈ [ratio 2ratio]

@test @inferred([0,1.2].+reshape([0,-2],1,1,2)) == reshape([0 -2; 1.2 -0.8],2,1,2)
rt = Base.return_types(.+, Tuple{Array{Float64, 3}, Array{Int, 1}})
@test length(rt) == 1 && rt[1] == Array{Float64, 3}
rt = Base.return_types(broadcast, Tuple{typeof(.+), Array{Float64, 3}, Array{Int, 3}})
@test length(rt) == 1 && rt[1] == Array{Float64, 3}
rt = Base.return_types(broadcast!, Tuple{Function, Array{Float64, 3}, Array{Float64, 3}, Array{Int, 1}})
@test length(rt) == 1 && rt[1] == Array{Float64, 3}

# f.(args...) syntax (#15032)
let x = [1,3.2,4.7], y = [3.5, pi, 1e-4], α = 0.2342
    @test sin.(x) == broadcast(sin, x)
    @test sin.(α) == broadcast(sin, α)
    @test factorial.(3) == broadcast(factorial, 3)
    @test atan2.(x, y) == broadcast(atan2, x, y)
    @test atan2.(x, y') == broadcast(atan2, x, y')
    @test atan2.(x, α) == broadcast(atan2, x, α)
    @test atan2.(α, y') == broadcast(atan2, α, y')
end

# issue 14725
let a = Number[2, 2.0, 4//2, 2+0im] / 2
    @test eltype(a) == Number
end
let a = Real[2, 2.0, 4//2] / 2
    @test eltype(a) == Real
end
let a = Real[2, 2.0, 4//2] / 2.0
    @test eltype(a) == Real
end

# issue 16164
let a = broadcast(Float32, [3, 4, 5])
    @test eltype(a) == Float32
end

# issue #4883
@test isa(broadcast(tuple, [1 2 3], ["a", "b", "c"]), Matrix{Tuple{Int,String}})
@test isa(broadcast((x,y)->(x==1?1.0:x,y), [1 2 3], ["a", "b", "c"]), Matrix{Tuple{Real,String}})
let a = length.(["foo", "bar"])
    @test isa(a, Vector{Int})
    @test a == [3, 3]
end
let a = sin.([1, 2])
    @test isa(a, Vector{Float64})
    @test a ≈ [0.8414709848078965, 0.9092974268256817]
end

# PR 16988
@test Base.promote_op(+, Bool) === Int
@test isa(broadcast(+, true), Array{Int,0})
@test Base.promote_op(Float64, Bool) === Float64
