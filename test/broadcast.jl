# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestBroadcastInternals

using Base.Broadcast: broadcast_indices, check_broadcast_indices,
                      check_broadcast_shape, newindex, _bcs
using Base: OneTo
using Test

@test @inferred(_bcs((3,5), (3,5))) == (3,5)
@test @inferred(_bcs((3,1), (3,5))) == (3,5)
@test @inferred(_bcs((3,),  (3,5))) == (3,5)
@test @inferred(_bcs((3,5), (3,)))  == (3,5)
@test_throws DimensionMismatch _bcs((3,5), (4,5))
@test_throws DimensionMismatch _bcs((3,5), (3,4))
@test @inferred(_bcs((-1:1, 2:5), (-1:1, 2:5))) == (-1:1, 2:5)
@test @inferred(_bcs((-1:1, 2:5), (1, 2:5)))    == (-1:1, 2:5)
@test @inferred(_bcs((-1:1, 1),   (1, 2:5)))    == (-1:1, 2:5)
@test @inferred(_bcs((-1:1,),     (-1:1, 2:5))) == (-1:1, 2:5)
@test_throws DimensionMismatch _bcs((-1:1, 2:6), (-1:1, 2:5))
@test_throws DimensionMismatch _bcs((-1:1, 2:5), (2, 2:5))

@test @inferred(broadcast_indices(zeros(3,4), zeros(3,4))) == (OneTo(3),OneTo(4))
@test @inferred(broadcast_indices(zeros(3,4), zeros(3)))   == (OneTo(3),OneTo(4))
@test @inferred(broadcast_indices(zeros(3),   zeros(3,4))) == (OneTo(3),OneTo(4))
@test @inferred(broadcast_indices(zeros(3), zeros(1,4), zeros(1))) == (OneTo(3),OneTo(4))

check_broadcast_indices((OneTo(3),OneTo(5)), zeros(3,5))
check_broadcast_indices((OneTo(3),OneTo(5)), zeros(3,1))
check_broadcast_indices((OneTo(3),OneTo(5)), zeros(3))
check_broadcast_indices((OneTo(3),OneTo(5)), zeros(3,5), zeros(3))
check_broadcast_indices((OneTo(3),OneTo(5)), zeros(3,5), 1)
check_broadcast_indices((OneTo(3),OneTo(5)), 5, 2)
@test_throws DimensionMismatch check_broadcast_indices((OneTo(3),OneTo(5)), zeros(2,5))
@test_throws DimensionMismatch check_broadcast_indices((OneTo(3),OneTo(5)), zeros(3,4))
@test_throws DimensionMismatch check_broadcast_indices((OneTo(3),OneTo(5)), zeros(3,4,2))
@test_throws DimensionMismatch check_broadcast_indices((OneTo(3),OneTo(5)), zeros(3,5), zeros(2))
check_broadcast_indices((-1:1, 6:9), 1)

check_broadcast_shape((-1:1, 6:9), (-1:1, 6:9))
check_broadcast_shape((-1:1, 6:9), (-1:1, 1))
check_broadcast_shape((-1:1, 6:9), (1, 6:9))
@test_throws DimensionMismatch check_broadcast_shape((-1:1, 6:9), (-1, 6:9))
@test_throws DimensionMismatch check_broadcast_shape((-1:1, 6:9), (-1:1, 6))

ci(x) = CartesianIndex(x)
@test @inferred(newindex(ci((2,2)), (true, true), (-1,-1)))   == ci((2,2))
@test @inferred(newindex(ci((2,2)), (true, false), (-1,-1)))  == ci((2,-1))
@test @inferred(newindex(ci((2,2)), (false, true), (-1,-1)))  == ci((-1,2))
@test @inferred(newindex(ci((2,2)), (false, false), (-1,-1))) == ci((-1,-1))
@test @inferred(newindex(ci((2,2)), (true,), (-1,-1)))   == ci((2,))
@test @inferred(newindex(ci((2,2)), (true,), (-1,)))   == ci((2,))
@test @inferred(newindex(ci((2,2)), (false,), (-1,))) == ci((-1,))
@test @inferred(newindex(ci((2,2)), (), ())) == ci(())

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
function as_sub(x::AbstractArray{T,3}) where T
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

bittest(f::Function, a...) = (@test f.(a...) == BitArray(broadcast(f, a...)))
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

    @test @inferred(broadcast(+, arr(eye(2)), arr([1, 4]))) == arr([2 1; 4 5])
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

    for f in ((==), (<) , (!=), (<=))
        bittest(f, arr(eye(2)), arr([1, 4]))
        bittest(f, arr(eye(2)), arr([1  4]))
        bittest(f, arr([0, 1]), arr([1  4]))
        bittest(f, arr([0  1]), arr([1, 4]))
        bittest(f, arr([1, 0]), arr([1, 4]))
        bittest(f, arr(rand(rb, n1, n2, n3)), arr(rand(rb, n1, n2, n3)))
        bittest(f, arr(rand(rb,  1, n2, n3)), arr(rand(rb, n1,  1, n3)))
        bittest(f, arr(rand(rb,  1, n2,  1)), arr(rand(rb, n1,  1, n3)))
        bittest(f, arr(bitrand(n1, n2, n3)), arr(bitrand(n1, n2, n3)))
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

@test @inferred(broadcast(+,[0,1.2],reshape([0,-2],1,1,2))) == reshape([0 -2; 1.2 -0.8],2,1,2)
rt = Base.return_types(broadcast, Tuple{typeof(+), Array{Float64, 3}, Array{Int, 1}})
@test length(rt) == 1 && rt[1] == Array{Float64, 3}
rt = Base.return_types(broadcast!, Tuple{Function, Array{Float64, 3}, Array{Float64, 3}, Array{Int, 1}})
@test length(rt) == 1 && rt[1] == Array{Float64, 3}

# f.(args...) syntax (#15032)
let x = [1,3.2,4.7], y = [3.5, pi, 1e-4], α = 0.2342
    @test sin.(x) == broadcast(sin, x)
    @test sin.(α) == broadcast(sin, α)
    @test sin.(3.2) == broadcast(sin, 3.2) == sin(3.2)
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
    @test eltype(a) == Float64
end

# issue 16164
let a = broadcast(Float32, [3, 4, 5])
    @test eltype(a) == Float32
end

# broadcasting scalars:
@test sin.(1) === broadcast(sin, 1) === sin(1)
@test (()->1234).() === broadcast(()->1234) === 1234

# issue #4883
@test isa(broadcast(tuple, [1 2 3], ["a", "b", "c"]), Matrix{Tuple{Int,String}})
@test isa(broadcast((x,y)->(x==1 ? 1.0 : x, y), [1 2 3], ["a", "b", "c"]), Matrix{Tuple{Real,String}})
let a = length.(["foo", "bar"])
    @test isa(a, Vector{Int})
    @test a == [3, 3]
end
let a = sin.([1, 2])
    @test isa(a, Vector{Float64})
    @test a ≈ [0.8414709848078965, 0.9092974268256817]
end

# PR #17300: loop fusion
@test (x->x+1).((x->x+2).((x->x+3).(1:10))) == collect(7:16)
let A = [sqrt(i)+j for i = 1:3, j=1:4]
    @test atan2.(log.(A), sum(A,1)) == broadcast(atan2, broadcast(log, A), sum(A, 1))
end
let x = sin.(1:10)
    @test atan2.((x->x+1).(x), (x->x+2).(x)) == broadcast(atan2, x.+1, x.+2)
    @test sin.(atan2.([x.+1,x.+2]...)) == sin.(atan2.(x.+1 ,x.+2)) == @. sin(atan2(x+1,x+2))
    @test sin.(atan2.(x, 3.7)) == broadcast(x -> sin(atan2(x,3.7)), x)
    @test atan2.(x, 3.7) == broadcast(x -> atan2(x,3.7), x) == broadcast(atan2, x, 3.7)
end
# Use side effects to check for loop fusion.
let g = Int[]
    f17300(x) = begin; push!(g, x); x+2; end
    f17300.(f17300.(f17300.(1:3)))
    @test g == [1,3,5, 2,4,6, 3,5,7]
    empty!(g)
    @. f17300(f17300(f17300(1:3)))
    @test g == [1,3,5, 2,4,6, 3,5,7]
end
# fusion with splatted args:
let x = sin.(1:10), a = [x]
    @test cos.(x) == cos.(a...)
    @test atan2.(x,x) == atan2.(a..., a...) == atan2.([x, x]...)
    @test atan2.(x, cos.(x)) == atan2.(a..., cos.(x)) == broadcast(atan2, x, cos.(a...)) == broadcast(atan2, a..., cos.(a...))
    @test ((args...)->cos(args[1])).(x) == cos.(x) == ((y,args...)->cos(y)).(x)
end
@test atan2.(3,4) == atan2(3,4) == (() -> atan2(3,4)).()
# fusion with keyword args:
let x = [1:4;]
    f17300kw(x; y=0) = x + y
    @test f17300kw.(x) == x
    @test f17300kw.(x, y=1) == f17300kw.(x; y=1) == f17300kw.(x; [(:y,1)]...) == x .+ 1
    @test f17300kw.(sin.(x), y=1) == f17300kw.(sin.(x); y=1) == sin.(x) .+ 1
    @test sin.(f17300kw.(x, y=1)) == sin.(f17300kw.(x; y=1)) == sin.(x .+ 1)
end

# issue #23236
let X = [[true,false],[false,true]]
    @test [.!x for x in X] == [[false,true],[true,false]]
end

# splice escaping of @.
let x = [4, -9, 1, -16]
    @test [2, 3, 4, 5] == @.(1 + sqrt($sort(abs(x))))
end

# interaction of @. with let
@test [1,4,9] == @. let x = [1,2,3]; x^2; end

# interaction of @. with for loops
let x = [1,2,3], y = x
    @. for i = 1:3
        y = y^2 # should convert to y .= y.^2
    end
    @test x == [1,256,6561]
end

# interaction of @. with function definitions
let x = [1,2,3]
    @. f(x) = x^2
    @test f(x) == [1,4,9]
end

# PR #17510: Fused in-place assignment
let x = [1:4;], y = x
    y .= 2:5
    @test y === x == [2:5;]
    y .= factorial.(x)
    @test y === x == [2,6,24,120]
    y .= 7
    @test y === x == [7,7,7,7]
    y .= factorial.(3)
    @test y === x == [6,6,6,6]
    f17510() = 9
    y .= f17510.()
    @test y === x == [9,9,9,9]
    y .-= 1
    @test y === x == [8,8,8,8]
    @. y -= 1:4          # @. should convert to .-=
    @test y === x == [7,6,5,4]
    x[1:2] .= 1
    @test y === x == [1,1,5,4]
    @. x[1:2] .+= [2,3]  # use .+= to make sure @. works with dotted assignment
    @test y === x == [3,4,5,4]
    @. x[:] .= 0         # use .= to make sure @. works with dotted assignment
    @test y === x == [0,0,0,0]
    @. x[2:end] = 1:3    # @. should convert to .=
    @test y === x == [0,1,2,3]
end
let a = [[4, 5], [6, 7]]
    a[1] .= 3
    @test a == [[3, 3], [6, 7]]
end
let d = Dict(:foo => [1,3,7], (3,4) => [5,9])
    d[:foo] .+= 2
    @test d[:foo] == [3,5,9]
    d[3,4] .-= 1
    @test d[3,4] == [4,8]
end
let identity = error, x = [1,2,3]
    x .= 1 # make sure it goes to broadcast!(Base.identity, ...), not identity
    @test x == [1,1,1]
end

# make sure scalars are inlined, which causes f.(x,scalar) to lower to a "thunk"
import Base.Meta: isexpr
@test isexpr(expand(Main, :(f.(x,y))), :call)
@test isexpr(expand(Main, :(f.(x,1))), :thunk)
@test isexpr(expand(Main, :(f.(x,1.0))), :thunk)
@test isexpr(expand(Main, :(f.(x,$π))), :thunk)
@test isexpr(expand(Main, :(f.(x,"hello"))), :thunk)
@test isexpr(expand(Main, :(f.(x,$("hello")))), :thunk)

# PR #17623: Fused binary operators
@test [true] .* [true] == [true]
@test [1,2,3] .|> (x->x+1) == [2,3,4]
let g = Int[], ⊕ = (a,b) -> let c=a+2b; push!(g, c); c; end
    @test [1,2,3] .⊕ [10,11,12] .⊕ [100,200,300] == [221,424,627]
    @test g == [21,221,24,424,27,627] # test for loop fusion
end

# Fused unary operators
@test .√[3,4,5] == sqrt.([3,4,5])
@test .![true, true, false] == [false, false, true]
@test .-[1,2,3] == -[1,2,3] == .+[-1,-2,-3] == [-1,-2,-3]

# PR 16988
@test Base.promote_op(+, Bool) === Int
@test isa(broadcast(+, [true]), Array{Int,1})

# issue #17304
let foo = [[1,2,3],[4,5,6],[7,8,9]]
    @test max.(foo...) == broadcast(max, foo...) == [7,8,9]
end

# Issue 17314
@test broadcast(x->log(log(log(x))), [1000]) == [log(log(log(1000)))]
let f17314 = x -> x < 0 ? false : x
    @test eltype(broadcast(f17314, 1:3)) === Int
    @test eltype(broadcast(f17314, -1:1)) === Integer
    @test eltype(broadcast(f17314, Int[])) == Union{Bool,Int}
end
let io = IOBuffer()
    broadcast(x->print(io,x), 1:5) # broadcast with side effects
    @test take!(io) == [0x31,0x32,0x33,0x34,0x35]
end

# Issue 18176
let f18176(a, b, c) = a + b + c
    @test f18176.(1.0:2, 3, 4) == f18176.(3.0, 1.0:2, 4.0) == broadcast(f18176, 3, 4, 1.0:2)
end

# Issue #17984
let A17984 = []
    @test isa(abs.(A17984), Array{Any,1})
end

# Issue #16966
@test parse.(Int, "1") == 1
@test parse.(Int, ["1", "2"]) == [1, 2]
@test trunc.((Int,), [1.2, 3.4]) == [1, 3]
@test abs.((1, -2)) == (1, 2)
@test broadcast(+, 1.0, (0, -2.0)) == (1.0,-1.0)
@test broadcast(+, 1.0, (0, -2.0), [1]) == [2.0, 0.0]
@test broadcast(*, ["Hello"], ", ", ["World"], "!") == ["Hello, World!"]
let s = "foo"
    @test s .* ["bar", "baz"] == ["foobar", "foobaz"] == "foo" .* ["bar", "baz"]
end

# Ensure that even strange constructors that break `T(x)::T` work with broadcast
struct StrangeType18623 end
StrangeType18623(x) = x
StrangeType18623(x,y) = (x,y)
@test @inferred(broadcast(StrangeType18623, 1:3)) == [1,2,3]
@test @inferred(broadcast(StrangeType18623, 1:3, 4:6)) == [(1,4),(2,5),(3,6)]

@test typeof(Int.(Number[1, 2, 3])) === typeof((x->Int(x)).(Number[1, 2, 3]))

@test @inferred(broadcast(CartesianIndex, 1:2)) == [CartesianIndex(1), CartesianIndex(2)]
@test @inferred(broadcast(CartesianIndex, 1:2, 3:4)) == [CartesianIndex(1,3), CartesianIndex(2,4)]

# Issue 18622
@test @inferred(broadcast(muladd, [1.0], [2.0], [3.0])) == [5.0]
@test @inferred(broadcast(tuple, 1:3, 4:6, 7:9)) == [(1,4,7), (2,5,8), (3,6,9)]

# 19419
@test @inferred(broadcast(round, Int, [1])) == [1]

# https://discourse.julialang.org/t/towards-broadcast-over-combinations-of-sparse-matrices-and-scalars/910
let
    f(A, n) = broadcast(x -> +(x, n), A)
    @test @inferred(f([1.0], 1)) == [2.0]
    g() = (a = 1; Base.Broadcast._broadcast_eltype(x -> x + a, 1.0))
    @test @inferred(g()) === Float64
end

# Ref as 0-dimensional array for broadcast
@test (-).(C_NULL, C_NULL)::UInt == 0
@test (+).(1, Ref(2)) == fill(3)
@test (+).(Ref(1), Ref(2)) == fill(3)
@test (+).([[0,2], [1,3]], Ref{Vector{Int}}([1,-1])) == [[1,1], [2,2]]

# Check that broadcast!(f, A) populates A via independent calls to f (#12277, #19722),
# and similarly for broadcast!(f, A, numbers...) (#19799).
@test let z = 1; A = broadcast!(() -> z += 1, zeros(2)); A[1] != A[2]; end
@test let z = 1; A = broadcast!(x -> z += x, zeros(2), 1); A[1] != A[2]; end

# broadcasting for custom AbstractArray
struct Array19745{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
Base.getindex(A::Array19745, i::Integer...) = A.data[i...]
Base.setindex!(A::Array19745, v::Any, i::Integer...) = setindex!(A.data, v, i...)
Base.size(A::Array19745) = size(A.data)

Base.Broadcast._containertype(::Type{T}) where {T<:Array19745} = Array19745

Base.Broadcast.promote_containertype(::Type{Array19745}, ::Type{Array19745}) = Array19745
Base.Broadcast.promote_containertype(::Type{Array19745}, ::Type{Array})      = Array19745
Base.Broadcast.promote_containertype(::Type{Array19745}, ct)                 = Array19745
Base.Broadcast.promote_containertype(::Type{Array}, ::Type{Array19745})      = Array19745
Base.Broadcast.promote_containertype(ct, ::Type{Array19745})                 = Array19745

Base.Broadcast.broadcast_indices(::Type{Array19745}, A)      = indices(A)
Base.Broadcast.broadcast_indices(::Type{Array19745}, A::Ref) = ()

getfield19745(x::Array19745) = x.data
getfield19745(x)             = x

function Base.Broadcast.broadcast_c(f, ::Type{Array19745}, A, Bs...)
    T     = Base.Broadcast._broadcast_eltype(f, A, Bs...)
    shape = Base.Broadcast.broadcast_indices(A, Bs...)
    dest = Array19745(Array{T}(Base.index_lengths(shape...)))
    return broadcast!(f, dest, A, Bs...)
end

@testset "broadcasting for custom AbstractArray" begin
    a  = randn(10)
    aa = Array19745(a)
    @test a .+ 1  == @inferred(aa .+ 1)
    @test a .* a' == @inferred(aa .* aa')
    @test isa(aa .+ 1, Array19745)
    @test isa(aa .* aa', Array19745)
end

# broadcast should only "peel off" one container layer
@test get.([Nullable(1), Nullable(2)]) == [1, 2]
let io = IOBuffer()
    broadcast(x -> print(io, x), [Nullable(1.0)])
    @test String(take!(io)) == "Nullable{Float64}(1.0)"
end

# Test that broadcast's promotion mechanism handles closures accepting more than one argument.
# (See issue #19641 and referenced issues and pull requests.)
let f() = (a = 1; Base.Broadcast._broadcast_eltype((x, y) -> x + y + a, 1.0, 1.0))
    @test @inferred(f()) == Float64
end

@testset "broadcast resulting in BitArray" begin
    let f(x) = x ? true : "false"
        ba = f.([true])
        @test ba isa BitArray
        @test ba == [true]
        a = f.([false])
        @test a isa Array{String}
        @test a == ["false"]
        @test f.([true, false]) == [true, "false"]
    end
end

# Test that broadcast treats type arguments as scalars, i.e. containertype yields Any,
# even for subtypes of abstract array. (https://github.com/JuliaStats/DataArrays.jl/issues/229)
@testset "treat type arguments as scalars, DataArrays issue 229" begin
    @test Base.Broadcast.containertype(AbstractArray) == Any
    @test broadcast(==, [1], AbstractArray) == BitArray([false])
    @test broadcast(==, 1, AbstractArray) == false
end

# Test that broadcasting identity where the input and output Array shapes do not match
# yields the correct result, not merely a partial copy. See pull request #19895 for discussion.
let N = 5
    @test iszero(ones(N, N) .= zeros(N, N))
    @test iszero(ones(N, N) .= zeros(N, 1))
    @test iszero(ones(N, N) .= zeros(1, N))
    @test iszero(ones(N, N) .= zeros(1, 1))
end

@testset "test broadcast for matrix of matrices" begin
    A = fill(zeros(2,2), 4, 4)
    A[1:3,1:3] .= [ones(2,2)]
    @test all(A[1:3,1:3] .== [ones(2,2)])
end

# Test that broadcast does not confuse eltypes. See also
# https://github.com/JuliaLang/julia/issues/21325
@testset "eltype confusion (#21325)" begin
    foo(x::Char, y::Int) = 0
    foo(x::String, y::Int) = "hello"
    @test broadcast(foo, "x", [1, 2, 3]) == ["hello", "hello", "hello"]

    @test isequal(
        [Set([1]), Set([2])] .∪ Set([3]),
        [Set([1, 3]), Set([2, 3])])

    @test isequal(@inferred(broadcast(foo, "world", Nullable(1))),
                  Nullable("hello"))
end

# Issue #21291
let t = (0, 1, 2)
    o = 1
    @test @inferred(broadcast(+, t, o)) == (1, 2, 3)
end
