# This file is a part of Julia. License is MIT: https://julialang.org/license

function args_morespecific(a, b)
    sp = (ccall(:jl_type_morespecific, Cint, (Any,Any), a, b) != 0)
    if sp  # make sure morespecific(a,b) implies !morespecific(b,a)
        @test ccall(:jl_type_morespecific, Cint, (Any,Any), b, a) == 0
    end
    return sp
end

# issue #8652
let
    a  = Tuple{Type{T1}, T1} where T1<:Integer
    b2 = Tuple{Type{T2}, Integer} where T2<:Integer
    @test args_morespecific(a, b2)
    @test !args_morespecific(b2, a)
    a  = Tuple{Type{T1}, Ptr{T1}} where T1<:Integer
    b2 = Tuple{Type{T2}, Ptr{Integer}} where T2<:Integer
    @test !args_morespecific(a, b2)
    @test  args_morespecific(b2, a)
end

# issue #11534
let
    t1 = Tuple{AbstractArray, Tuple{Vararg{Base.RangeIndex}}}
    t2 = Tuple{Array, T} where T<:Tuple{Vararg{Base.RangeIndex}}
    @test !args_morespecific(t1, t2)
    @test  args_morespecific(t2, t1)
end

let
    a = Tuple{Array{T,N}, Vararg{Int,N}} where T where N
    b = Tuple{Array,Int}
    @test  args_morespecific(a, b)
    @test !args_morespecific(b, a)
    a = Tuple{Array, Vararg{Int,N}} where N
    @test !args_morespecific(a, b)
    @test  args_morespecific(b, a)
end

# another specificity issue
_z_z_z_(x, y) = 1
_z_z_z_(::Int, ::Int, ::Vector) = 2
_z_z_z_(::Int, c...) = 3
@test _z_z_z_(1, 1, []) == 2

@test  args_morespecific(Tuple{T,Vararg{T}} where T<:Number,  Tuple{Number,Number,Vararg{Number}})
@test !args_morespecific(Tuple{Number,Number,Vararg{Number}}, Tuple{T,Vararg{T}} where T<:Number)

@test args_morespecific(Tuple{Array{T} where T<:Union{Float32,Float64,ComplexF32,ComplexF64}, Any},
                        Tuple{Array{T} where T<:Real, Any})

@test  args_morespecific(Tuple{1,T} where T, Tuple{Any})
@test  args_morespecific(Tuple{T} where T, Tuple{T,T} where T)
@test !args_morespecific(Type{T} where T<:Integer, Type{Any})

# issue #21016
@test args_morespecific(Tuple{IO, Core.TypeofBottom}, Tuple{IO, Type{T}} where T<:Number)

# issue #21382
@test args_morespecific(Tuple{Type{Pair{A,B} where B}} where A, Tuple{DataType})
@test args_morespecific(Tuple{Union{Int,String},Type{Pair{A,B} where B}} where A, Tuple{Integer,UnionAll})

# PR #21750
let A = Tuple{Any, Tuple{Vararg{Integer,N} where N}},
    B = Tuple{Any, Tuple{Any}},
    C = Tuple{Any, Tuple{}}
    @test args_morespecific(A, B)
    @test args_morespecific(C, A)
    @test args_morespecific(C, B)
end

# with bound varargs
_bound_vararg_specificity_1(::Type{Array{T,N}}, d::Vararg{Int, N}) where {T,N} = 0
_bound_vararg_specificity_1(::Type{Array{T,1}}, d::Int) where {T} = 1
@test _bound_vararg_specificity_1(Array{Int,1}, 1) == 1
@test _bound_vararg_specificity_1(Array{Int,2}, 1, 1) == 0

# issue #21710
@test args_morespecific(Tuple{Array}, Tuple{AbstractVector})
@test args_morespecific(Tuple{Matrix}, Tuple{AbstractVector})

# Method specificity
begin
    local f, A
    f(dims::Tuple{}, A::AbstractArray{T,0}) where {T} = 1
    f(dims::NTuple{N,Int}, A::AbstractArray{T,N}) where {T,N} = 2
    f(dims::NTuple{M,Int}, A::AbstractArray{T,N}) where {T,M,N} = 3
    A = zeros(2,2)
    @test f((1,2,3), A) == 3
    @test f((1,2), A) == 2
    @test f((), reshape([1])) == 1
    f(dims::NTuple{N,Int}, A::AbstractArray{T,N}) where {T,N} = 4
    @test f((1,2), A) == 4
    @test f((1,2,3), A) == 3
end

# a method specificity issue
c99991(::Type{T},x::T) where {T} = 0
c99991(::Type{UnitRange{T}},x::StepRangeLen{T}) where {T} = 1
c99991(::Type{UnitRange{T}},x::AbstractRange{T}) where {T} = 2
@test c99991(UnitRange{Float64}, 1.0:2.0) == 1
@test c99991(UnitRange{Int}, 1:2) == 2

# issue #17016, method specificity involving vararg tuples
T_17016{N} = Tuple{Any,Any,Vararg{Any,N}}
f17016(f, t::T_17016) = 0
f17016(f, t1::Tuple) = 1
@test f17016(0, (1,2,3)) == 0

@test  args_morespecific(Tuple{Type{Any}, Any}, Tuple{Type{T}, Any} where T<:VecElement)
@test !args_morespecific((Tuple{Type{T}, Any} where T<:VecElement), Tuple{Type{Any}, Any})

@test !args_morespecific(Tuple{Type{T}, Tuple{Any, Vararg{Any, N} where N}} where T<:Tuple{Any, Vararg{Any, N} where N},
                         Tuple{Type{Any}, Any})
@test !args_morespecific(Tuple{Type{T}, Tuple{Any, Vararg{Any, N} where N}} where T<:Tuple{Any, Vararg{Any, N} where N},
                         Tuple{Type{Tuple}, Tuple})
@test !args_morespecific(Tuple{Type{T}, T} where T<:Tuple{Any, Vararg{Any, N} where N},
                         Tuple{Type{T}, Any} where T<:VecElement)

@test args_morespecific(Tuple{Any, Tuple{}, Tuple{}}, Tuple{Any, Tuple{Any}})
@test args_morespecific(Tuple{Any, Tuple{Any}, Tuple{Any}}, Tuple{Any, Tuple{Any, Any}})
@test args_morespecific(Tuple{Any, Vararg{Tuple{}, N} where N}, Tuple{Any, Tuple{Any}})

@test  args_morespecific(Tuple{T, T} where T<:AbstractFloat, Tuple{T, T, T} where T<:AbstractFloat)
@test  args_morespecific(Tuple{T, Real, T} where T<:AbstractFloat, Tuple{T, T} where T<:Real)
@test  args_morespecific(Tuple{Real, Real}, Tuple{T, T, T} where T <: Real)
@test !args_morespecific(Tuple{Real, Real, Real}, Tuple{T, T, T} where T <: Real)
@test !args_morespecific(Tuple{Real, Real, Vararg{Real}}, Tuple{T, T, T} where T <: Real)
@test  args_morespecific(Tuple{Real, Real, Vararg{Int}}, Tuple{T, T, T} where T <: Real)

@test  args_morespecific(Tuple{Type{Base.Some{T}}} where T, Tuple{Type{T}, Any} where T)
@test !args_morespecific(Tuple{Type{Base.Some{T}}, T} where T, Tuple{Type{Base.Some{T}}} where T)

@test  args_morespecific(Tuple{Union{Base.StepRange{T, S} where S, Base.StepRangeLen{T, T, S} where S},
                               Union{Base.StepRange{T, S} where S, Base.StepRangeLen{T, T, S} where S}} where T,
                         Tuple{T, T} where T<:Union{Base.StepRangeLen, Base.LinRange})

@test args_morespecific(Tuple{Type{Tuple}, Any, Any},
                        Tuple{Type{Tuple{Vararg{E, N} where N}}, Any, Any} where E)

@test args_morespecific(Tuple{Type{Tuple{}}, Tuple{}},
                        Tuple{Type{T}, T} where T<:Tuple{Any, Vararg{Any, N} where N})

@test args_morespecific(Tuple{Type{CartesianIndex{N}}} where N,
                        Tuple{Type{CartesianIndex{N}},Vararg{Int,N}} where N)

# issue #22164
let A = Tuple{Type{D},D} where D<:Pair,
    B = Tuple{Type{Any}, Any},
    C = Tuple{Type{Pair}, Pair}
    @test  args_morespecific(C, A)
    @test !args_morespecific(A, B)
    @test !args_morespecific(C, B)
end

# issue #22338
let A = Tuple{Ref, Tuple{T}} where T,
    B = Tuple{Ref{T}, Tuple{Vararg{T}}} where T,
    C = Tuple{Ref{T}, Tuple{T}} where T
    @test  args_morespecific(C, A)
    @test  args_morespecific(C, B)
    @test !args_morespecific(A, B)
    @test !args_morespecific(B, A)
end

# issue #22339
let A = Tuple{T, Array{T, 1}} where T,
    B = Tuple{T} where T,
    C = Tuple{T} where T<:AbstractFloat
    @test args_morespecific(B, A)
    @test args_morespecific(C, B)
    @test args_morespecific(C, A)
end

# issue #22908
f22908(::Union) = 2
f22908(::Type{Union{Int, Float32}}) = 1
@test f22908(Union{Int, Float32}) == 1

let x = Type{Union{Tuple{T}, Tuple{Ptr{T}, Ptr{T}, Any}} where T},
    y = Type{Union{Tuple{T}, Tuple{Array{T, N} where N, Any, Array{T, N} where N, Any, Any}} where T}
    @test !args_morespecific(x, y)
    @test !args_morespecific(y, x)
    @test !args_morespecific(x.parameters[1], y.parameters[1])
    @test !args_morespecific(y.parameters[1], x.parameters[1])
end

let A = Tuple{Array{T,N}, Vararg{Int,N}} where {T,N},
    B = Tuple{Array, Int},
    C = Tuple{AbstractArray, Int, Array}
    @test args_morespecific(A, B)
    @test args_morespecific(B, C)
    @test args_morespecific(A, C)
end

# transitivity issue found in #26915
let A = Tuple{Vector, AbstractVector},
    B = Tuple{AbstractVecOrMat{T}, AbstractVector{T}} where T,
    C = Tuple{AbstractVecOrMat{T}, AbstractVecOrMat{T}} where T
    @test args_morespecific(A, B)
    @test args_morespecific(B, C)
    @test args_morespecific(A, C)
end

# issue #27361
f27361(::M) where M <: Tuple{2} = nothing
f27361(::M) where M <: Tuple{3} = nothing
@test length(methods(f27361)) == 2

# specificity of TypeofBottom
@test args_morespecific(Tuple{Core.TypeofBottom}, Tuple{DataType})
@test args_morespecific(Tuple{Core.TypeofBottom}, Tuple{Type{<:Tuple}})

@test  args_morespecific(Tuple{Type{Any}, Type}, Tuple{Type{T}, Type{T}} where T)
@test !args_morespecific(Tuple{Type{Any}, Type}, Tuple{Type{T}, Type{T}} where T<:Union{})

# issue #22592
abstract type Colorant22592{T,N} end
abstract type Color22592{T, N} <: Colorant22592{T,N} end
abstract type AbstractRGB22592{T} <: Color22592{T,3} end
AbstractGray22592{T} = Color22592{T,1}
MathTypes22592{T,C} = Union{AbstractRGB22592{T},AbstractGray22592{T}}
@test !args_morespecific(Tuple{MathTypes22592}, Tuple{AbstractGray22592})
@test !args_morespecific(Tuple{MathTypes22592, MathTypes22592}, Tuple{AbstractGray22592})

@test args_morespecific(Union{Set,Dict,Vector}, Union{Vector,AbstractSet})
