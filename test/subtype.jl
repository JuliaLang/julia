# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: Bottom
using Test
using LinearAlgebra

macro UnionAll(var, expr)
    Expr(:where, esc(expr), esc(var))
end

const issub = (<:)
issub_strict(@nospecialize(x),@nospecialize(y)) = issub(x,y) && !issub(y,x)
isequal_type(@nospecialize(x),@nospecialize(y)) = issub(x,y) && issub(y,x)
notequal_type(@nospecialize(x),@nospecialize(y)) = !isequal_type(x, y)

_type_intersect(@nospecialize(x), @nospecialize(y)) = ccall(:jl_intersect_types, Any, (Any, Any), x, y)

intersection_env(@nospecialize(x), @nospecialize(y)) = ccall(:jl_type_intersection_with_env, Any, (Any,Any), x, y)

# level 1: no varags, union, UnionAll
function test_1()
    @test issub_strict(Int, Integer)
    @test issub_strict(Array{Int,1}, AbstractArray{Int,1})

    @test isequal_type(Int, Int)
    @test isequal_type(Integer, Integer)
    @test isequal_type(Array{Int,1}, Array{Int,1})
    @test isequal_type(AbstractArray{Int,1}, AbstractArray{Int,1})

    @test issub_strict(Tuple{Int,Int}, Tuple{Integer,Integer})
    @test issub_strict(Tuple{Array{Int,1}}, Tuple{AbstractArray{Int,1}})

    @test isequal_type(Tuple{Integer,Integer}, Tuple{Integer,Integer})

    @test !issub(Tuple{Int,Int}, Tuple{Int})
    @test !issub(Tuple{Int}, Tuple{Integer,Integer})

    @test !issub(Array{Int,1}, Array{Integer,1})
end

# level 2: varargs
function test_2()
    @test issub_strict(Tuple{Int,Int}, Tuple{Vararg{Int}})
    @test issub_strict(Tuple{Int,Int}, Tuple{Int,Vararg{Int}})
    @test issub_strict(Tuple{Int,Int}, Tuple{Int,Vararg{Integer}})
    @test issub_strict(Tuple{Int,Int}, Tuple{Int,Int,Vararg{Integer}})
    @test issub_strict(Tuple{Int,Vararg{Int}}, Tuple{Vararg{Int}})
    @test issub_strict(Tuple{Int,Int,Int}, Tuple{Vararg{Int}})
    @test issub_strict(Tuple{Int,Int,Int}, Tuple{Integer,Vararg{Int}})
    @test issub_strict(Tuple{Int}, Tuple{Any})
    @test issub_strict(Tuple{}, Tuple{Vararg{Any}})

    @test isequal_type(Tuple{Int}, Tuple{Int})
    @test isequal_type(Tuple{Vararg{Integer}}, Tuple{Vararg{Integer}})

    @test !issub(Tuple{}, Tuple{Int, Vararg{Int}})
    @test !issub(Tuple{Int}, Tuple{Int, Int, Vararg{Int}})

    @test !issub(Tuple{Int, Tuple{Real, Integer}}, Tuple{Vararg{Int}})

    @test isequal_type(Tuple{Int,Int}, Tuple{Vararg{Int,2}})

    @test Tuple{Int,Vararg{Int,2}} == Tuple{Int,Int,Int}
    @test Tuple{Int,Vararg{Int,2}} === Tuple{Int,Int,Int}
    @test Tuple{Any, Any} === Tuple{Vararg{Any,2}}
    @test Tuple{Int,Vararg{Int,2}} == Tuple{Int,Int,Vararg{Int,1}}
    @test Tuple{Int,Vararg{Int,2}} == Tuple{Int,Int,Int,Vararg{Int,0}}
    @test !(Tuple{Int,Vararg{Int,2}} <: Tuple{Int,Int,Int,Vararg{Int,1}})
    @test Tuple{Int,Vararg{Int}} == Tuple{Int,Vararg{Int}}
    @test (@UnionAll N Tuple{Int,Vararg{Int,N}}) == (@UnionAll N Tuple{Int,Vararg{Int,N}})

    @test issub_strict(Tuple{Tuple{Int,Int},Tuple{Int,Int}}, Tuple{NTuple{N,Int},NTuple{N,Int}} where N)
    @test !issub(Tuple{Tuple{Int,Int},Tuple{Int,}}, Tuple{NTuple{N,Int},NTuple{N,Int}} where N)
    @test NTuple{0} === Tuple{}

    @test !issub(Tuple{Val{3}, Vararg{Val{3}}}, Tuple{Vararg{Val{N}, N}} where N)

    @test issub_strict(Tuple{Int,Int}, Tuple{Int,Int,Vararg{Int,N}} where N)
    @test issub_strict(Tuple{Int,Int}, Tuple{E,E,Vararg{E,N}} where E where N)

    @test issub(Type{Tuple{VecElement{Bool}}}, (Type{Tuple{Vararg{VecElement{T},N}}} where T where N))

    @test isequal_type(Type{Tuple{Vararg{Int,N}} where N}, Type{Tuple{Vararg{Int}}})
    @test Type{Tuple{Vararg{Int,N}} where N} !== Type{Tuple{Vararg{Int}}}
end

function test_diagonal()
    @test !issub(Tuple{Integer,Integer}, @UnionAll T Tuple{T,T})
    @test issub(Tuple{Integer,Int}, (@UnionAll T @UnionAll S<:T Tuple{T,S}))
    @test issub(Tuple{Integer,Int}, (@UnionAll T @UnionAll T<:S<:T Tuple{T,S}))
    @test issub(Tuple{Integer,Int,Int}, (@UnionAll T @UnionAll T<:S<:T Tuple{T,S,S}))

    @test issub_strict((@UnionAll R Tuple{R,R}),
                       (@UnionAll T @UnionAll S Tuple{T,S}))
    @test issub_strict((@UnionAll R Tuple{R,R}),
                       (@UnionAll T @UnionAll S<:T Tuple{T,S}))
    @test issub_strict((@UnionAll R Tuple{R,R}),
                       (@UnionAll T @UnionAll T<:S<:T Tuple{T,S}))
    @test issub_strict((@UnionAll R Tuple{R,R}),
                       (@UnionAll T @UnionAll S>:T Tuple{T,S}))

    @test !issub(Tuple{Real,Real}, @UnionAll T<:Real Tuple{T,T})

    @test issub((@UnionAll S<:Int (@UnionAll R<:AbstractString Tuple{S,R,Vector{Any}})),
                (@UnionAll T Tuple{T, T, Array{T,1}}))

    @test issub_strict(Tuple{String, Real, Ref{Number}},
                       (@UnionAll T Tuple{Union{T,String}, T, Ref{T}}))

    @test issub_strict(Tuple{String, Real},
                       (@UnionAll T Tuple{Union{T,String}, T}))

    @test !issub(      Tuple{Real, Real},
                       (@UnionAll T Tuple{Union{T,String}, T}))

    @test issub_strict(Tuple{Int, Int},
                       (@UnionAll T Tuple{Union{T,String}, T}))

    # don't consider a diagonal variable concrete if it already has an abstract lower bound
    @test isequal_type(Tuple{Vararg{A}} where A>:Integer,
                       Tuple{Vararg{A}} where A>:Integer)

    # issue #24166
    @test !issub(Tuple{T, T, Ref{T}} where T, Tuple{S, S, Ref{Q} where Q} where S)
    @test !issub(Tuple{T, T, Ref{T}} where T, Tuple{S, S, Ref{Q} where Q} where S<:Integer)
    @test !issub(Tuple{T, T, Ref{T}} where T, Tuple{S, S, Ref{Q} where Q} where S<:Int)
    @test  issub(Tuple{T, T, Ref{T}} where T<:Int, Tuple{S, S, Ref{Q} where Q} where S)
    @test !issub(Tuple{T, T, Ref{T}} where T>:Int, Tuple{S, S, Ref{Q} where Q} where S)
    @test !issub(Tuple{T, T, Ref{T}} where T>:Integer, Tuple{S, S, Ref{Q} where Q} where S)
    @test !issub(Tuple{T, T, Ref{T}} where T>:Any, Tuple{S, S, Ref{Q} where Q} where S)

    @test  issub(Tuple{T, T} where Int<:T<:Int, Tuple{T, T} where Int<:T<:Int)
    @test  issub(Tuple{T, T} where T>:Int, Tuple{T, T} where T>:Int)
    @test  issub(Tuple{Tuple{T, T} where T>:Int}, Tuple{Tuple{T, T} where T>:Int})
    @test  issub(Tuple{Tuple{T, T} where T>:Int}, Tuple{Tuple{T, T}} where T>:Int)
    @test  issub(Tuple{Tuple{T, T}} where T>:Int, Tuple{Tuple{T, T} where T>:Int})
    @test  issub(Vector{Tuple{T, T} where Number<:T<:Number},
                 Vector{Tuple{Number, Number}})

    @test !issub(Type{Tuple{T,Any} where T},   Type{Tuple{T,T}} where T)
    @test !issub(Type{Tuple{T,Any,T} where T}, Type{Tuple{T,T,T}} where T)
    @test_broken issub(Type{Tuple{T} where T},       Type{Tuple{T}} where T)
    @test  issub(Ref{Tuple{T} where T},        Ref{Tuple{T}} where T)
    @test !issub(Type{Tuple{T,T} where T},     Type{Tuple{T,T}} where T)
    @test !issub(Type{Tuple{T,T,T} where T},   Type{Tuple{T,T,T}} where T)
    @test  isequal_type(Ref{Tuple{T, T} where Int<:T<:Int},
                        Ref{Tuple{S, S}} where Int<:S<:Int)

    let A = Tuple{Int,Int8,Vector{Integer}},
        B = Tuple{T,T,Vector{T}} where T>:Integer,
        C = Tuple{T,T,Vector{Union{Integer,T}}} where T
        @test A <: B
        @test B == C
        @test A <: C
        @test Tuple{Int,Int8,Vector{Any}} <: C
    end

    # #26108
    @test !issub((Tuple{T, T, Array{T, 1}} where T), Tuple{T, T, Any} where T)

    # #26716
    @test !issub((Union{Tuple{Int,Bool}, Tuple{P,Bool}} where P), Tuple{Union{T,Int}, T} where T)

    @test issub_strict(Tuple{Ref{Tuple{N,N}}, Ref{N}} where N,
                       Tuple{Ref{Tuple{N1,N1}}, Ref{N2}} where {N1, N2})
    @test !issub(Tuple{Type{Tuple{Vararg{T}} where T <: Integer}, Tuple{Float64, Int}},
                 Tuple{Type{Tuple{Vararg{T}}}, Tuple{Vararg{T}}} where T)

    # non-types
    @test issub_strict(Tuple{3,3}, NTuple)
    @test !issub(Tuple{3,4}, NTuple)
end

# level 3: UnionAll
function test_3()
    @test issub_strict(Array{Int,1}, @UnionAll T Vector{T})
    @test issub_strict((@UnionAll T Pair{T,T}), Pair)
    @test issub(Pair{Int,Int8}, Pair)
    @test issub(Pair{Int,Int8}, (@UnionAll S Pair{Int,S}))

    @test !issub((@UnionAll T<:Real T), (@UnionAll T<:Integer T))

    @test isequal_type((@UnionAll T Tuple{T,T}), (@UnionAll R Tuple{R,R}))

    @test !issub((@UnionAll T<:Integer @UnionAll S<:Number Tuple{T,S}),
                 (@UnionAll T<:Integer @UnionAll S<:Number Tuple{S,T}))

    @test issub_strict((@UnionAll T Tuple{Array{T},Array{T}}),
                       Tuple{Array, Array})

    AUA = Array{(@UnionAll T Array{T,1}), 1}
    UAA = (@UnionAll T Array{Array{T,1}, 1})

    @test !issub(AUA, UAA)
    @test !issub(UAA, AUA)
    @test !isequal_type(AUA, UAA)

    @test issub_strict((@UnionAll T Int), (@UnionAll T<:Integer Integer))

    @test isequal_type((@UnionAll T @UnionAll S Tuple{T, Tuple{S}}),
                       (@UnionAll T Tuple{T, @UnionAll S Tuple{S}}))

    @test !issub((@UnionAll T Pair{T,T}), Pair{Int,Int8})
    @test !issub((@UnionAll T Pair{T,T}), Pair{Int,Int})

    @test isequal_type((@UnionAll T Tuple{T}), Tuple{Any})
    @test isequal_type((@UnionAll T<:Real Tuple{T}), Tuple{Real})

    @test  issub(Tuple{Array{Integer,1}, Int},
                 @UnionAll T<:Integer @UnionAll S<:T Tuple{Array{T,1},S})

    @test !issub(Tuple{Array{Integer,1}, Real},
                 @UnionAll T<:Integer Tuple{Array{T,1},T})

    @test !issub(Tuple{Int,String,Vector{Integer}},
                 @UnionAll T Tuple{T, T, Array{T,1}})
    @test !issub(Tuple{String,Int,Vector{Integer}},
                 @UnionAll T Tuple{T, T, Array{T,1}})
    @test !issub(Tuple{Int,String,Vector{Tuple{Integer}}},
                 @UnionAll T Tuple{T,T,Array{Tuple{T},1}})

    @test issub(Tuple{Int,String,Vector{Any}},
                @UnionAll T Tuple{T, T, Array{T,1}})

    @test isequal_type(Array{Int,1}, Array{(@UnionAll T<:Int T), 1})
    @test isequal_type(Array{Tuple{Any},1}, Array{(@UnionAll T Tuple{T}), 1})

    @test isequal_type(Array{Tuple{Int,Int},1},
                       Array{(@UnionAll T<:Int Tuple{T,T}), 1})
    @test !issub(Array{Tuple{Int,Integer},1},
                 Array{(@UnionAll T<:Integer Tuple{T,T}), 1})

    @test !issub(Pair{Int,Int8}, (@UnionAll T Pair{T,T}))

    @test !issub(Tuple{Array{Int,1}, Integer},
                 @UnionAll T<:Integer Tuple{Array{T,1},T})

    @test !issub(Tuple{Integer, Array{Int,1}},
                 @UnionAll T<:Integer Tuple{T, Array{T,1}})

    @test !issub(Pair{Array{Int,1},Integer}, @UnionAll T Pair{Array{T,1},T})
    @test  issub(Pair{Array{Int,1},Int}, @UnionAll T Pair{Array{T,1},T})

    @test  issub(Tuple{Integer,Int}, @UnionAll T<:Integer @UnionAll S<:T Tuple{T,S})
    @test !issub(Tuple{Integer,Int}, @UnionAll T<:Int     @UnionAll S<:T Tuple{T,S})
    @test !issub(Tuple{Integer,Int}, @UnionAll T<:String  @UnionAll S<:T Tuple{T,S})

    @test issub(Tuple{Float32,Array{Float32,1}},
                @UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})

    @test !issub(Tuple{Float32,Array{Float64,1}},
                 @UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})

    @test issub(Tuple{Float32,Array{Real,1}},
                @UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})

    @test !issub(Tuple{Number,Array{Real,1}},
                 @UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})

    @test issub((@UnionAll Int<:T<:Integer T), @UnionAll T<:Real T)
    @test issub((@UnionAll Int<:T<:Integer Array{T,1}),
                (@UnionAll T<:Real Array{T,1}))

    @test  issub((@UnionAll Int<:T<:Integer T), (@UnionAll Integer<:T<:Real T))
    @test !issub((@UnionAll Int<:T<:Integer Array{T,1}), (@UnionAll Integer<:T<:Real Array{T,1}))

    X = (@UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S})
    Y = (@UnionAll A<:Real @UnionAll B<:AbstractArray{A,1} Tuple{A,B})
    @test isequal_type(X,Y)
    Z = (@UnionAll A<:Real @UnionAll B<:AbstractArray{A,1} Tuple{Real,B})
    @test issub_strict(X,Z)

    @test issub_strict((@UnionAll T @UnionAll S<:T Pair{T,S}),
                       (@UnionAll T @UnionAll S    Pair{T,S}))
    @test issub_strict((@UnionAll T @UnionAll S>:T Pair{T,S}),
                       (@UnionAll T @UnionAll S    Pair{T,S}))

    # these would be correct if the diagonal rule applied to type vars occurring
    # only once in covariant position.
    #@test issub_strict((@UnionAll T Tuple{Ref{T}, T}),
    #                   (@UnionAll T @UnionAll S<:T Tuple{Ref{T},S}))
    #@test issub_strict((@UnionAll T Tuple{Ref{T}, T}),
    #                   (@UnionAll T @UnionAll S<:T @UnionAll R<:S Tuple{Ref{T},R}))

    @test isequal_type((@UnionAll T Tuple{Ref{T}, T}),
                       (@UnionAll T @UnionAll T<:S<:T Tuple{Ref{T},S}))
    @test issub_strict((@UnionAll T Tuple{Ref{T}, T}),
                       (@UnionAll T @UnionAll S>:T Tuple{Ref{T}, S}))

    A = @UnionAll T Tuple{T,Ptr{T}}
    B = @UnionAll T Tuple{Ptr{T},T}

    C = @UnionAll T>:Ptr @UnionAll S>:Ptr    Tuple{Ptr{T},Ptr{S}}
    D = @UnionAll T>:Ptr @UnionAll S>:Ptr{T} Tuple{Ptr{T},Ptr{S}}
    E = @UnionAll T>:Ptr @UnionAll S>:Ptr{T} Tuple{Ptr{S},Ptr{T}}

    @test !issub(A, B)
    @test !issub(B, A)
    @test issub_strict(C, A)
    @test issub_strict(C, B)
    @test issub_strict(C, D)
    @test issub_strict(Union{D,E}, A)
    @test issub_strict(Union{D,E}, B)
    @test issub_strict((@UnionAll T>:Ptr @UnionAll Ptr<:S<:Ptr    Tuple{Ptr{T},Ptr{S}}),
                       (@UnionAll T>:Ptr @UnionAll S>:Ptr{T} Tuple{Ptr{T},Ptr{S}}))
    @test !issub((@UnionAll T>:Ptr @UnionAll S>:Ptr    Tuple{Ptr{T},Ptr{S}}),
                 (@UnionAll T>:Ptr @UnionAll Ptr{T}<:S<:Ptr Tuple{Ptr{T},Ptr{S}}))

    @test !issub((@UnionAll T>:Integer @UnionAll S>:Ptr Tuple{Ptr{T},Ptr{S}}), B)

    @test  issub((@UnionAll T>:Ptr @UnionAll S>:Integer Tuple{Ptr{T},Ptr{S}}), B)

    # issue #23327
    @test !issub((Type{AbstractArray{Array{T}} where T}), Type{AbstractArray{S}} where S)
    @test !issub((Val{AbstractArray{Array{T}} where T}), Val{AbstractArray{T}} where T)
    @test !issub((Array{Array{Array{T}} where T}), Array{Array{T}} where T)
    @test !issub((Array{Array{T, 1}, 1} where T), AbstractArray{Vector})

    @test !issub((Ref{Pair{Pair{T, R}, R} where R} where T),
                 (Ref{Pair{A,          B} where B} where A))
    @test !issub((Ref{Pair{Pair{A, B}, B} where B} where A),
                 (Ref{Pair{A,          B2} where B2 <: B} where A where B))

    @test !issub(Tuple{Type{Vector{T}} where T, Vector{Float64}}, Tuple{Type{T}, T} where T)
    @test !issub(Tuple{Vector{Float64}, Type{Vector{T}} where T}, Tuple{T, Type{T}} where T)
    @test !issub(Tuple{Type{Ref{T}} where T, Vector{Float64}}, Tuple{Ref{T}, T} where T)

    @test !issub(Tuple{Type{Ref{T}} where T, Ref{Float64}}, Tuple{Type{T},T} where T)
end

# level 4: Union
function test_4()
    @test isequal_type(Union{Bottom,Bottom}, Bottom)

    @test issub_strict(Int, Union{Int,String})
    @test issub_strict(Union{Int,Int8}, Integer)

    @test isequal_type(Union{Int,Int8}, Union{Int,Int8})

    @test isequal_type(Union{Int,Integer}, Integer)

    @test isequal_type(Tuple{Union{Int,Int8},Int16}, Union{Tuple{Int,Int16},Tuple{Int8,Int16}})

    @test issub_strict(Tuple{Int,Int8,Int}, Tuple{Vararg{Union{Int,Int8}}})
    @test issub_strict(Tuple{Int,Int8,Int}, Tuple{Vararg{Union{Int,Int8,Int16}}})

    # nested unions
    @test !issub(Union{Int,Ref{Union{Int,Int8}}}, Union{Int,Ref{Union{Int8,Int16}}})

    A = Int64; B = Int8
    C = Int16; D = Int32
    @test  issub(Union{Union{A,Union{A,Union{B,C}}}, Union{D,Bottom}},
                 Union{Union{A,B},Union{C,Union{B,D}}})
    @test !issub(Union{Union{A,Union{A,Union{B,C}}}, Union{D,Bottom}},
                 Union{Union{A,B},Union{C,Union{B,A}}})

    @test isequal_type(Union{Union{A,B,C}, Union{D}}, Union{A,B,C,D})
    @test isequal_type(Union{Union{A,B,C}, Union{D}}, Union{A,Union{B,C},D})
    @test isequal_type(Union{Union{Union{Union{A}},B,C}, Union{D}},
                       Union{A,Union{B,C},D})

    @test issub_strict(Union{Union{A,C}, Union{D}}, Union{A,B,C,D})

    @test !issub(Union{Union{A,B,C}, Union{D}}, Union{A,C,D})

    # obviously these unions can be simplified, but when they aren't there's trouble
    X = Union{Union{A,B,C},Union{A,B,C},Union{A,B,C},Union{A,B,C},
              Union{A,B,C},Union{A,B,C},Union{A,B,C},Union{A,B,C}}
    Y = Union{Union{D,B,C},Union{D,B,C},Union{D,B,C},Union{D,B,C},
              Union{D,B,C},Union{D,B,C},Union{D,B,C},Union{A,B,C}}
    @test issub_strict(X,Y)
end

# level 5: union and UnionAll
function test_5()
    u = Union{Int8,Int}

    @test issub(Tuple{String,Array{Int,1}},
                (@UnionAll T Union{Tuple{T,Array{T,1}}, Tuple{T,Array{Int,1}}}))

    @test issub(Tuple{Union{Vector{Int},Vector{Int8}}},
                @UnionAll T Tuple{Array{T,1}})

    @test !issub(Tuple{Union{Vector{Int},Vector{Int8}},Vector{Int}},
                 @UnionAll T Tuple{Array{T,1}, Array{T,1}})

    @test !issub(Tuple{Union{Vector{Int},Vector{Int8}},Vector{Int8}},
                 @UnionAll T Tuple{Array{T,1}, Array{T,1}})

    @test !issub(Vector{Int}, @UnionAll T>:u Array{T,1})
    @test  issub(Vector{Integer}, @UnionAll T>:u Array{T,1})
    @test  issub(Vector{Union{Int,Int8}}, @UnionAll T>:u Array{T,1})

    @test issub((@UnionAll Int<:T<:u Array{T,1}), (@UnionAll Int<:T<:u Array{T,1}))

    # with varargs
    @test !issub(Array{Tuple{Array{Int},Array{Vector{Int16}},Array{Vector{Int}},Array{Int}}},
                 @UnionAll T<:(@UnionAll S Tuple{Vararg{Union{Array{S}, Array{Array{S,1}}}}}) Array{T})

    @test  issub(Array{Tuple{Array{Int},Array{Vector{Int}},Array{Vector{Int}},Array{Int}}},
                 @UnionAll T<:(@UnionAll S Tuple{Vararg{Union{Array{S}, Array{Array{S,1}}}}}) Array{T})

    @test !issub(Tuple{Array{Int},Array{Vector{Int16}},Array{Vector{Int}},Array{Int}},
                 @UnionAll S Tuple{Vararg{Union{Array{S},Array{Array{S,1}}}}})

    @test  issub(Tuple{Array{Int},Array{Vector{Int}},Array{Vector{Int}},Array{Int}},
                 @UnionAll S Tuple{Vararg{Union{Array{S},Array{Array{S,1}}}}})

    B = @UnionAll S<:u Tuple{S, Tuple{Any,Any,Any}, Ref{S}}
    # these tests require renaming in issub_unionall
    @test  issub((@UnionAll T<:B Tuple{Int8, T, Ref{Int8}}), B)
    @test !issub((@UnionAll T<:B Tuple{Int8, T, Ref{T}}),    B)

    # the `convert(Type{T},T)` pattern, where T is a Union
    # required changing priority of unions and vars
    @test issub(Tuple{Array{u,1},Int}, @UnionAll T Tuple{Array{T,1}, T})
    @test issub(Tuple{Array{u,1},Int}, @UnionAll T @UnionAll S<:T Tuple{Array{T,1}, S})

    @test !issub(Ref{Union{Ref{Int},Ref{Int8}}}, @UnionAll T Ref{Ref{T}})
    @test  issub(Tuple{Union{Ref{Int},Ref{Int8}}}, @UnionAll T Tuple{Ref{T}})
    @test !issub(Ref{Union{Ref{Int},Ref{Int8}}}, Union{Ref{Ref{Int}}, Ref{Ref{Int8}}})

    @test isequal_type(Ref{Tuple{Union{Int,Int8},Int16}}, Ref{Union{Tuple{Int,Int16},Tuple{Int8,Int16}}})
    @test isequal_type(Ref{T} where T<:Tuple{Union{Int,Int8},Int16},
                       Ref{T} where T<:Union{Tuple{Int,Int16},Tuple{Int8,Int16}})

    @test isequal_type(Ref{Tuple{Union{Int,Int8},Int16,T}} where T,
                       Ref{Union{Tuple{Int,Int16,S},Tuple{Int8,Int16,S}}} where S)

    # issue #32726
    @test Tuple{Type{Any}, Int, Float64, String} <: Tuple{Type{T}, Vararg{T}} where T
end

# tricky type variable lower bounds
function test_6()
    @test  issub((@UnionAll S<:Int (@UnionAll R<:String Tuple{S,R,Vector{Any}})),
                 (@UnionAll T Tuple{T, T, Array{T,1}}))

    @test !issub((@UnionAll S<:Int (@UnionAll R<:String Tuple{S,R,Vector{Integer}})),
                 (@UnionAll T Tuple{T, T, Array{T,1}}))

    t = @UnionAll T Tuple{T,T,Ref{T}}
    @test isequal_type(t, @UnionAll S Tuple{S,S,Ref{S}})

    @test !issub((@UnionAll T Tuple{T,String,Ref{T}}), (@UnionAll T Tuple{T,T,Ref{T}}))

    @test !issub((@UnionAll T Tuple{T,Ref{T},String}), (@UnionAll T Tuple{T,Ref{T},T}))

    i = Int; ai = Integer
    @test isequal_type((@UnionAll i<:T<:i   Ref{T}), Ref{i})
    @test isequal_type((@UnionAll ai<:T<:ai Ref{T}), Ref{ai})

    # Pair{T,S} <: Pair{T,T} can be true with certain bounds
    @test issub_strict((@UnionAll i<:T<:i @UnionAll i<:S<:i Pair{T,S}),
                       @UnionAll T Pair{T,T})

    @test issub_strict(Tuple{i, Ref{i}},
                       (@UnionAll T @UnionAll S<:T Tuple{S,Ref{T}}))

    @test !issub(Tuple{Real, Ref{i}},
                 (@UnionAll T @UnionAll S<:T Tuple{S,Ref{T}}))

    # S >: T
    @test issub_strict(Tuple{Real, Ref{i}},
                       (@UnionAll T @UnionAll S>:T Tuple{S,Ref{T}}))

    @test !issub(Tuple{Ref{i}, Ref{ai}},
                 (@UnionAll T @UnionAll S>:T Tuple{Ref{S},Ref{T}}))

    @test issub_strict(Tuple{Ref{Real}, Ref{ai}},
                       (@UnionAll T @UnionAll S>:T Tuple{Ref{S},Ref{T}}))

    @test issub_strict(Tuple{Real, Ref{Tuple{i}}},
                       (@UnionAll T @UnionAll S>:T Tuple{S,Ref{Tuple{T}}}))

    @test !issub(Tuple{Ref{Tuple{i}}, Ref{Tuple{ai}}},
                 (@UnionAll T @UnionAll S>:T Tuple{Ref{Tuple{S}},Ref{Tuple{T}}}))

    @test issub_strict(Tuple{Ref{Tuple{Real}}, Ref{Tuple{ai}}},
                       (@UnionAll T @UnionAll S>:T Tuple{Ref{Tuple{S}},Ref{Tuple{T}}}))

    # (@UnionAll x<:T<:x Q{T}) == Q{x}
    @test isequal_type(Ref{Ref{i}}, Ref{@UnionAll i<:T<:i Ref{T}})
    @test isequal_type(Ref{Ref{i}}, @UnionAll i<:T<:i Ref{Ref{T}})
    @test isequal_type((@UnionAll i<:T<:i Ref{Ref{T}}), Ref{@UnionAll i<:T<:i Ref{T}})
    @test !issub((@UnionAll i<:T<:i Ref{Ref{T}}), Ref{@UnionAll T<:i Ref{T}})

    u = Union{Int8,Int64}
    A = Ref{Bottom}
    B = @UnionAll S<:u Ref{S}
    @test issub(Ref{B}, @UnionAll A<:T<:B Ref{T})

    C = @UnionAll S<:u S
    @test issub(Ref{C}, @UnionAll u<:T<:u Ref{T})

    BB = @UnionAll S<:Bottom S
    @test issub(Ref{B}, @UnionAll BB<:U<:B Ref{U})
end

# uncategorized
function test_7()
    @test isequal_type(Ref{Union{Int16, T}} where T, Ref{Union{Int16, S}} where S)
    @test isequal_type(Pair{Union{Int16, T}, T} where T, Pair{Union{Int16, S}, S} where S)
end

function test_Type()
    @test issub_strict(DataType, Type)
    @test issub_strict(Union, Type)
    @test issub_strict(UnionAll, Type)
    @test issub_strict(typeof(Bottom), Type)
    @test !issub(TypeVar, Type)
    @test !issub(Type, TypeVar)
    @test !issub(DataType, @UnionAll T<:Number Type{T})
    @test issub_strict(Type{Int}, DataType)
    @test !issub((@UnionAll T<:Integer Type{T}), DataType)
    @test isequal_type(Type{AbstractArray}, Type{AbstractArray})
    @test !issub(Type{Int}, Type{Integer})
    @test issub((@UnionAll T<:Integer Type{T}), (@UnionAll T<:Number Type{T}))
    @test isa(Int, @UnionAll T<:Number Type{T})
    @test !isa(DataType, @UnionAll T<:Number Type{T})

    @test !(DataType <: (@UnionAll T<:Type Type{T}))
    @test isa(DataType, (@UnionAll T<:Type Type{T}))

    @test isa(Tuple{},Type{Tuple{}})
    @test !(Tuple{Int,} <: (@UnionAll T<:Tuple Type{T}))
    @test isa(Tuple{Int}, (@UnionAll T<:Tuple Type{T}))

    @test !isa(Int, Type{>:String})
    @test  isa(Union{Int,String}, Type{>:String})
    @test  isa(Any, Type{>:String})

    # this matches with T==DataType, since DataType is concrete
    @test  issub(Tuple{Type{Int},Type{Int8}}, Tuple{T,T} where T)
    @test !issub(Tuple{Type{Int},Type{Union{}}}, Tuple{T,T} where T)

    # issue #20476
    @test issub(Tuple{Type{Union{Type{UInt32}, Type{UInt64}}}, Type{UInt32}}, Tuple{Type{T},T} where T)

    @test isequal_type(Core.TypeofBottom, Type{Union{}})
    @test issub(Core.TypeofBottom, Type{T} where T<:Real)
end

# old subtyping tests from test/core.jl
function test_old()
    @test Int8 <: Integer
    @test Int32 <: Integer
    @test Tuple{Int8,Int8} <: Tuple{Integer,Integer}
    @test !(AbstractArray{Float64,2} <: AbstractArray{Number,2})
    @test !(AbstractArray{Float64,1} <: AbstractArray{Float64,2})
    @test Tuple{Integer,Vararg{Integer}} <: Tuple{Integer,Vararg{Real}}
    @test Tuple{Integer,Float64,Vararg{Integer}} <: Tuple{Integer,Vararg{Number}}
    @test Tuple{Integer,Float64} <: Tuple{Integer,Vararg{Number}}
    @test Tuple{Int32,} <: Tuple{Vararg{Number}}
    @test Tuple{} <: Tuple{Vararg{Number}}
    @test !(Tuple{Vararg{Int32}} <: Tuple{Int32,})
    @test !(Tuple{Vararg{Int32}} <: Tuple{Number,Integer})
    @test !(Tuple{Vararg{Integer}} <: Tuple{Integer,Integer,Vararg{Integer}})
    @test !(Array{Int8,1} <: Array{Any,1})
    @test !(Array{Any,1} <: Array{Int8,1})
    @test Array{Int8,1} <: Array{Int8,1}
    @test !(Type{Bottom} <: Type{Int32})
    @test !(Vector{Float64} <: Vector{Union{Float64,Float32}})

    @test !isa(Array,Type{Any})
    @test Type{Complex} <: UnionAll
    @test isa(Complex,Type{Complex})
    @test !(Type{Ptr{Bottom}} <: Type{Ptr})
    @test !(Type{Rational{Int}} <: Type{Rational})
    @test Tuple{} <: Tuple{Vararg}
    @test Tuple{Int,Int} <: Tuple{Vararg}
    @test Tuple{} <: @UnionAll N NTuple{N}
    @test !(Type{Tuple{}} <: Type{Tuple{Vararg}})
    @test   Type{Tuple{}} <: (@UnionAll N Type{NTuple{N}})

    @test !(Type{Array{Integer}} <: Type{AbstractArray{Integer}})
    @test !(Type{Array{Integer}} <: Type{@UnionAll T<:Integer Array{T}})

    # issue #6561
    # TODO: note that NTuple now means "tuples of all the same type"
    #@test (Array{Tuple} <: Array{NTuple})
    @test issub_strict(NTuple, Tuple)
    @test issub_strict(NTuple, Tuple{Vararg})
    @test isequal_type(Tuple, Tuple{Vararg})
    #@test (Array{Tuple{Vararg{Any}}} <: Array{NTuple})
    #@test (Array{Tuple{Vararg}} <: Array{NTuple})
    @test !(Type{Tuple{Nothing}} <: Tuple{Type{Nothing}})
end

const easy_menagerie =
    Any[Any, Int, Int8, Integer, Real,
        Array{Int,1}, AbstractArray{Int,1},
        Tuple{Int,Vararg{Integer}}, Tuple{Integer,Vararg{Int}}, Tuple{},
        Union{Int,Int8},
        (@UnionAll T Array{T,1}),
        (@UnionAll T Pair{T,T}),
        (@UnionAll T @UnionAll S Pair{T,S}),
        Pair{Int,Int8},
        (@UnionAll S Pair{Int,S}),
        (@UnionAll T Tuple{T,T}),
        (@UnionAll T<:Integer Tuple{T,T}),
        (@UnionAll T @UnionAll S Tuple{T,S}),
        (@UnionAll T<:Integer @UnionAll S<:Number Tuple{T,S}),
        (@UnionAll T<:Integer @UnionAll S<:Number Tuple{S,T}),
        Array{(@UnionAll T Array{T,1}),1},
        (@UnionAll T Array{Array{T,1},1}),
        Array{(@UnionAll T<:Int T), 1},
        (@UnionAll T<:Real @UnionAll S<:AbstractArray{T,1} Tuple{T,S}),
        Union{Int,Ref{Union{Int,Int8}}},
        ]

const hard_menagerie =
    Any[(@UnionAll T Union{Tuple{T,Array{T,1}}, Tuple{T,Array{Int,1}}})]

function add_variants!(types)
    new = Any[]
    for T in types
        push!(new, Ref{T})
        push!(new, Tuple{T})
        push!(new, Tuple{T,T})
        push!(new, Tuple{Vararg{T}})
        push!(new, @UnionAll S<:T S)
        push!(new, @UnionAll S<:T Ref{S})
    end
    append!(types, new)
end

add_variants!(easy_menagerie)
add_variants!(hard_menagerie)
push!(easy_menagerie, Bottom)
push!(easy_menagerie, Ref{Bottom})
push!(easy_menagerie, @UnionAll N NTuple{N,Bottom})
push!(easy_menagerie, @UnionAll S<:Bottom Ref{S})

const menagerie = [easy_menagerie; hard_menagerie]

function test_properties()
    x→y = !x || y
    ¬T = @UnionAll X>:T Ref{X}

    for T in menagerie
        # top and bottom identities
        @test issub(Bottom, T)
        @test issub(T, Any)
        @test issub(T, Bottom) → isequal_type(T, Bottom)
        @test issub(Any, T) → isequal_type(T, Any)

        # unionall identity
        @test isequal_type(T, @UnionAll S<:T S)
        @test isequal_type(Ref{T}, @UnionAll T<:U<:T Ref{U})

        # equality under renaming
        if isa(T, UnionAll)
            lb, ub = T.var.lb, T.var.ub
            @test isequal_type(T, (@UnionAll lb<:Y<:ub T{Y}))
        end

        # inequality under wrapping
        @test !isequal_type(T, Ref{T})

        for S in menagerie
            issubTS = issub(T, S)
            # transitivity
            if issubTS
                for R in menagerie
                    if issub(S, R)
                        @test issub(T, R)  # issub(T,S) ∧ issub(S,R) → issub(T,R)
                        @test issub(Ref{S}, @UnionAll T<:U<:R Ref{U})
                    end
                end
            end

            # union subsumption
            @test isequal_type(T, Union{T,S}) → issub(S, T)

            # invariance
            @test isequal_type(T, S) == isequal_type(Ref{T}, Ref{S})

            # covariance
            if T !== Bottom && S !== Bottom
                @test issubTS == issub(Tuple{T}, Tuple{S})
                @test issubTS == issub(Tuple{Vararg{T}}, Tuple{Vararg{S}})
                @test issubTS == issub(Tuple{T}, Tuple{Vararg{S}})
            end

            # pseudo-contravariance
            @test issubTS == issub(¬S, ¬T)
        end
    end
end

macro testintersect(a, b, result)
    if isa(result, Expr) && result.head === :call && length(result.args) == 2 && result.args[1] === :!
        result = result.args[2]
        cmp = :(!=)
    else
        cmp = :(==)
    end
    a = esc(a)
    b = esc(b)
    result = esc(result)
    Base.remove_linenums!(quote
        # test real intersect
        @test $cmp(_type_intersect($a, $b), $result)
        @test $cmp(_type_intersect($b, $a), $result)
        # test simplified intersect
        if !($result === Union{})
            @test typeintersect($a, $b) != Union{}
            @test typeintersect($b, $a) != Union{}
        end
    end)
end

abstract type IT4805_2{N, T} end
abstract type AbstractThing{T,N} end
mutable struct ConcreteThing{T<:AbstractFloat,N} <: AbstractThing{T,N}
end
mutable struct A11136 end
mutable struct B11136 end
abstract type Foo11367 end

abstract type AbstractTriangular{T,S<:AbstractMatrix} <: AbstractMatrix{T} end
struct UpperTriangular{T,S<:AbstractMatrix} <: AbstractTriangular{T,S} end
struct UnitUpperTriangular{T,S<:AbstractMatrix} <: AbstractTriangular{T,S} end

struct SIQ20671{T<:Number,m,kg,s,A,K,mol,cd,rad,sr} <: Number
    val::T
end

function test_intersection()
    @testintersect(Vector{Float64}, Vector{Union{Float64,Float32}}, Bottom)

    @testintersect(Array{Bottom}, (@UnionAll T AbstractArray{T}), !Bottom)

    @testintersect(Tuple{Type{Ptr{UInt8}}, Ptr{Bottom}},
                   (@UnionAll T Tuple{Type{Ptr{T}},Ptr{T}}), Bottom)

    @testintersect(Tuple{AbstractRange{Int},Tuple{Int,Int}}, (@UnionAll T Tuple{AbstractArray{T},Dims}),
                   Tuple{AbstractRange{Int},Tuple{Int,Int}})

    @testintersect((@UnionAll Integer<:T<:Number Array{T}), (@UnionAll T<:Number Array{T}),
                   (@UnionAll Integer<:T<:Number Array{T}))

    @testintersect((@UnionAll Integer<:T<:Number Array{T}), (@UnionAll T<:Real Array{T}),
                   (@UnionAll Integer<:T<:Real Array{T}))

    @testintersect((@UnionAll Integer<:T<:Number Array{T}), (@UnionAll T<:String Array{T}),
                   Bottom)

    @testintersect((@UnionAll Integer<:T<:Number Array{T}), (@UnionAll String<:T<:AbstractString Array{T}),
                   Bottom)

    @testintersect((@UnionAll T<:Number Array{T}), (@UnionAll T<:String Array{T}),
                   Array{Bottom})

    @testintersect((@UnionAll T Tuple{T, AbstractArray{T}}), Tuple{Number, Array{Int,1}},
                   Tuple{Int, Array{Int,1}})

    @testintersect((@UnionAll T Tuple{T, AbstractArray{T}}), Tuple{Int, Array{Number,1}},
                   Tuple{Int, Array{Number,1}})

    # TODO: improve this result
    #@testintersect((@UnionAll S Tuple{S,Vector{S}}), (@UnionAll T<:Real Tuple{T,AbstractVector{T}}),
    #               (@UnionAll S<:Real Tuple{S,Vector{S}}))
    @testintersect((@UnionAll S Tuple{S,Vector{S}}), (@UnionAll T<:Real Tuple{T,AbstractVector{T}}),
                   (@UnionAll S<:Real Tuple{Real,Vector{S}}))

    # typevar corresponding to a type it will end up being neither greater than nor
    # less than
    @testintersect((@UnionAll T Tuple{T, Ref{T}}), Tuple{Array{Int}, Ref{AbstractVector}},
                   Tuple{Array{Int,1}, Ref{AbstractVector}})

    @testintersect((@UnionAll T Tuple{T, AbstractArray{T}}), Tuple{Any, Array{Number,1}},
                   Tuple{Number, Array{Number,1}})
    @testintersect((@UnionAll T Tuple{Array{T}, Array{T}}), Tuple{Array, Array{Any}}, !Bottom)

    @testintersect((@UnionAll T Tuple{T,T}), Tuple{Real, Real}, (@UnionAll T<:Real Tuple{T,T}))
    @testintersect((@UnionAll T Tuple{T}), Tuple{Real}, Tuple{Real})
    @testintersect((@UnionAll T Tuple{T,T}), Tuple{Union{Float64,Int64},Int64}, Tuple{Int64,Int64})
    @testintersect((@UnionAll T Tuple{T,T}), Tuple{Int64,Union{Float64,Int64}}, Tuple{Int64,Int64})
    @testintersect((@UnionAll Z Tuple{Z,Z}), (@UnionAll T<:Integer @UnionAll S<:Number Tuple{T,S}),
                   @UnionAll Z<:Integer Tuple{Z,Z})
    @testintersect((@UnionAll Z Pair{Z,Z}), (@UnionAll T<:Integer @UnionAll S<:Number Pair{T,S}),
                   @UnionAll Z<:Integer Pair{Z,Z})

    @testintersect((@UnionAll T<:Vector Type{T}), (@UnionAll N Type{@UnionAll S<:Number Array{S,N}}),
                   Type{@UnionAll S<:Number Array{S,1}})

    @testintersect((@UnionAll T Tuple{Type{Array{T,1}},Array{T,1}}),
                   Tuple{Type{AbstractVector},Vector{Int}}, Bottom)

    @testintersect(Tuple{Type{Vector{ComplexF64}}, AbstractVector},
                   (@UnionAll T @UnionAll S @UnionAll N Tuple{Type{Array{T,N}}, Array{S,N}}),
                   Tuple{Type{Vector{ComplexF64}},Vector})

    @testintersect(Tuple{Type{Vector{ComplexF64}}, AbstractArray},
                   (@UnionAll T @UnionAll S @UnionAll N Tuple{Type{Array{T,N}}, Array{S,N}}),
                   Tuple{Type{Vector{ComplexF64}},Vector})

    @testintersect(Type{Array}, Type{AbstractArray}, Bottom)

    @testintersect(Type{Tuple{Bool,Vararg{Int}}}, Type{@UnionAll T Tuple{Vararg{T}}}, Bottom)
    @testintersect(Type{Tuple{Bool,Vararg{Int}}}, Type{@UnionAll T Tuple{T,Vararg{T}}}, Bottom)
    @testintersect((@UnionAll T Tuple{Vararg{T}}), Tuple{Float64,Int}, Bottom)

    @testintersect((@UnionAll T Tuple{Rational{T},T}), Tuple{Rational{Integer},Int}, Tuple{Rational{Integer},Int})

    @testintersect((@UnionAll T Pair{T,Ptr{T}}), (@UnionAll S Pair{Ptr{S},S}), Bottom)
    let A = Tuple{T,Ptr{T}} where T,
        B = Tuple{Ptr{S},S} where S,
        correct = Union{Tuple{Ptr{T},Ptr{S}} where S>:Ptr{T} where T>:Ptr,
                        Tuple{Ptr{S},Ptr{T}} where S>:Ptr{T} where T>:Ptr}
        # TODO: get the correct answer. for now check that `typeintersect`
        # at least gives a conservative answer.
        @test typeintersect(B, A) == typeintersect(A, B) >: correct
    end

    @testintersect((@UnionAll N Tuple{NTuple{N,Integer},NTuple{N,Integer}}),
                   Tuple{Tuple{Integer,Integer}, Tuple{Vararg{Integer}}},
                   Tuple{Tuple{Integer,Integer}, Tuple{Integer,Integer}})
    @testintersect((@UnionAll N Tuple{NTuple{N,Integer},NTuple{N,Integer}}),
                   Tuple{Tuple{Vararg{Integer}}, Tuple{Integer,Integer}},
                   Tuple{Tuple{Integer,Integer}, Tuple{Integer,Integer}})

    @test isequal_type(typeintersect((@UnionAll N Tuple{NTuple{N,Any},Array{Int,N}}),
                                     Tuple{Tuple{Int,Vararg{Int}},Array}),
                       @UnionAll N Tuple{Tuple{Int,Vararg{Int}},Array{Int,N}})

    @testintersect((@UnionAll N Tuple{NTuple{N,Any},Array{Int,N}}),
                   Tuple{Tuple{Int,Vararg{Int}},Array{Int,2}},
                   Tuple{Tuple{Int,Int}, Array{Int,2}})
    @testintersect(Type{Any},Type{Complex}, Bottom)
    @testintersect(Type{Any},(@UnionAll T<:Real Type{T}), Bottom)

    @testintersect(Type{Function},Union,Bottom)
    @testintersect(Type{Int32}, DataType, Type{Int32})

    @testintersect(DataType, Type, !Bottom)
    @testintersect(Union, Type, !Bottom)
    @testintersect(DataType, Type{Int}, !Bottom)
    @testintersect(DataType, (@UnionAll T<:Int Type{T}), !Bottom)
    @testintersect(DataType, (@UnionAll T<:Integer Type{T}), !Bottom)
    @testintersect(Tuple{Vararg{Int}}, Tuple{Vararg{Bool}}, Tuple{})
    @testintersect(Type{Tuple{Vararg{Int}}}, Type{Tuple{Vararg{Bool}}}, Bottom)
    @testintersect(Tuple{Bool,Vararg{Int}}, Tuple{Vararg{Bool}}, Tuple{Bool})

    let M = @UnionAll T<:Union{Float32,Float64} Matrix{T}
        @testintersect(AbstractArray, M, M)
    end

    @testintersect((@UnionAll N Tuple{Array{Int,N},Vararg{Int,N}}), Tuple{Vector{Int},Real,Real,Real}, Bottom)

    @testintersect((@UnionAll N Tuple{Array{Int,N},Vararg{Int,N}}), Tuple{Array{Int,0}}, Tuple{Array{Int,0}})
    @testintersect((@UnionAll N Tuple{Array{Int,N},Vararg{Int,N}}), Tuple{Array{Int,2}}, Bottom)

    @testintersect(Tuple{Int,Vararg{Int}}, Tuple{Int,Int,Int,Vararg{Float64}}, Tuple{Int,Int,Int})
    @testintersect(Tuple{Int,Vararg{Int}}, Tuple{Int,Vararg{Float64}}, Tuple{Int})
    @testintersect((@UnionAll N Tuple{Array{Int,N},Vararg{Int,N}}),
                   Tuple{Matrix{Int},Int,Int,Vararg{Float64}},
                   Tuple{Matrix{Int},Int,Int})
    @testintersect((@UnionAll N Tuple{Array{Int,N},Vararg{Int,N}}),
                   Tuple{Matrix{Int},Int,Vararg{Float64}}, Bottom)

    @testintersect(Tuple{Array{Any,1}, Tuple{Int64, Int64, Vararg{Int64}}},
                   Tuple{Array{T,N}, Tuple{Vararg{Int64,N}}} where N where T,
                   Bottom)

    @testintersect((@UnionAll T<:Union{Float64,Array{Float64,1}} T), Real, Float64)

    # issue #4805
    @testintersect((@UnionAll T<:Int Type{IT4805_2{1,T}}),
                   (@UnionAll S<:(@UnionAll N IT4805_2{N,Int}) Type{S}),
                   !Bottom)

    # issue #8851
    @testintersect((@UnionAll T AbstractThing{T,2}),
                   ConcreteThing,
                   (@UnionAll T<:AbstractFloat ConcreteThing{T,2}))

    # issue #11136 and #11367
    @testintersect((@UnionAll T Tuple{T, T}), (@UnionAll TB<:B11136 Tuple{A11136, TB}), Bottom)
    @testintersect((@UnionAll T Tuple{T, T}), (@UnionAll T2<:Foo11367 Tuple{Type{BigInt}, T2}), Bottom)

    # PR #12058
    @testintersect((@UnionAll N NTuple{N,Int}), (@UnionAll N NTuple{N,Float64}), Tuple{})

    @testintersect((@UnionAll T Tuple{Type{T},T}), Tuple{Type{Type{Float64}},Type{Int}}, Bottom)

    @testintersect((@UnionAll T T), Type{Int8}, Type{Int8})
    # issue #14482
    @testintersect((@UnionAll T Tuple{T}), Tuple{Type{Int8}}, Tuple{Type{Int8}})

    @testintersect((@UnionAll T Tuple{Union{Int,T}, Union{Vector{T},Vector{String}}}),
                   Tuple{Integer, Vector{Int8}},
                   Tuple{Union{Int,Int8}, Vector{Int8}})
    @testintersect((@UnionAll T Tuple{Union{Int,T}, Union{Vector{T},Vector{String}}}),
                   Tuple{Int8, Vector{String}},
                   Tuple{Int8, Vector{String}})
    @testintersect((@UnionAll T Tuple{Union{Int,T}, Union{Vector{T},Vector{String}}}),
                   Tuple{Int, Vector{Int8}},
                   Tuple{Int, Vector{Int8}})
    @testintersect((            Tuple{Union{Int,String}, Union{Ref{Int}, Ref{String}}}),
                   (@UnionAll T Tuple{T,                 Union{Ref{T},   Ref{String}}}),
                   Union{Tuple{Int,    Union{Ref{Int},Ref{String}}},
                         Tuple{String, Ref{String}}})

    @testintersect((@UnionAll Z<:(@UnionAll T @UnionAll S Tuple{T,S}) Ref{Z}),
                   (@UnionAll X<:(@UnionAll T Tuple{T,T}) Ref{X}),
                   (@UnionAll X<:(@UnionAll T Tuple{T,T}) Ref{X}))
    @testintersect(Ref{@UnionAll T @UnionAll S Tuple{T,S}},
                   Ref{@UnionAll T Tuple{T,T}}, Bottom)

    # both of these answers seem acceptable
    #@testintersect(Tuple{T,T} where T<:Union{UpperTriangular, UnitUpperTriangular},
    #               Tuple{AbstractArray{T,N}, AbstractArray{T,N}} where N where T,
    #               Union{Tuple{T,T} where T<:UpperTriangular{T1},
    #                     Tuple{T,T} where T<:UnitUpperTriangular{T1}} where T)
    @testintersect(Tuple{T,T} where T<:Union{UpperTriangular, UnitUpperTriangular},
                   Tuple{AbstractArray{T,N}, AbstractArray{T,N}} where N where T,
                   Tuple{T,T} where {T1, T<:Union{UpperTriangular{T1}, UnitUpperTriangular{T1}}})

    @testintersect(DataType, Type, DataType)
    @testintersect(DataType, Type{T} where T<:Integer, Type{T} where T<:Integer)
    @testintersect(Union{DataType,Int}, Type, DataType)
    @testintersect(Union{DataType,Int}, Type{T} where T, DataType)
    # test typeintersect wrapper as well as _type_intersect
    @test typeintersect(Union{DataType,Int}, Type) === DataType
    @test typeintersect(Union{DataType,Int}, Type{T} where T) === DataType

    # since TypeofBottom is a singleton we can deduce its intersection with Type{...}
    @testintersect(Core.TypeofBottom, (Type{T} where T<:Tuple), Type{Union{}})

    # since this T is inside the invariant ctor Type{}, we allow T == Any here
    @testintersect((Type{Tuple{Vararg{T}}} where T), Type{Tuple}, Type{Tuple})

    # TODO: improve this
    @testintersect(Tuple{Type{S}, Tuple{Any, Vararg{Any}}} where S<:Tuple{Any, Vararg{Any}},
                   Tuple{Type{T}, T} where T,
                   Tuple{Type{S}, Tuple{Any, Vararg{Any}}} where S<:Tuple{Any, Vararg{Any}})

    # part of issue #20450
    @testintersect(Tuple{Array{Ref{T}, 1}, Array{Pair{M, V}, 1}} where V where T where M,
                   Tuple{Array{Ref{T}, 1}, Array{Pair{M, T}, 1}, SS} where T where M where SS,
                   Union{})

    @testintersect(Tuple{Array{Ref{T}, 1}, Array{Pair{M, V}, 1}, Int} where V where T where M,
                   Tuple{Array{Ref{T}, 1}, Array{Pair{M, T}, 1}, Any} where T where M,
                   Tuple{Array{Ref{T}, 1}, Array{Pair{M, T}, 1}, Int} where T where M)

    @testintersect(Tuple{Int, Ref{Pair{K,V}}} where V where K,
                   Tuple{Any, Ref{Pair{T,T}} where T },
                   Tuple{Int, Ref{Pair{T,T}} where T })

    @test_broken isequal_type(_type_intersect(Tuple{T,T} where T,
                                              Union{Tuple{S,Array{Int64,1}},Tuple{S,Array{S,1}}} where S),
                              Union{Tuple{Vector{Int64},Vector{Int64}},
                                    Tuple{Vector{T},Vector{T}} where T>:Vector})

    # part of issue #20344
    @testintersect(Tuple{Type{Tuple{Vararg{T}}}, Tuple} where T,
                   Tuple{Type{Tuple{Vararg{T, N}}} where N where T, Any},
                   Bottom)
    @testintersect(Type{NTuple{N,UnitRange}} where N,
                   Type{Tuple{Vararg{UnitRange}}},
                   Bottom)

    @testintersect(Type{NTuple{Z,UnitRange}} where Z,
                   Type{NTuple{Z,String}} where Z,
                   Type{Tuple{}})

    # first union component sets N==0, but for the second N is unknown
    _, E = intersection_env(Tuple{Tuple{Vararg{Int}}, Any},
                            Tuple{Union{Base.DimsInteger{N},Base.Indices{N}}, Int} where N)
    @test length(E)==1 && isa(E[1],TypeVar)

    @testintersect(Tuple{Dict{Int,Int}, Ref{Pair{K,V}}} where V where K,
                   Tuple{AbstractDict{Int,Int}, Ref{Pair{T,T}} where T},
                   Tuple{Dict{Int,Int}, Ref{Pair{K,K}}} where K)

    # issue #20643
    @testintersect(Tuple{Ref{Pair{p2,T2}}, Pair{p1,Pair}} where T2 where p2 where p1,
                   Tuple{Ref{Pair{p3,T3}}, Pair{p3}} where T3 where p3,
                   Tuple{Ref{Pair{p1,T2}}, Pair{p1,Pair}} where T2 where p1)

    # issue #20998
    _, E = intersection_env(Tuple{Int,Any,Any}, Tuple{T,T,S} where {T,S})
    @test length(E) == 2 && E[1] == Int && isa(E[2], TypeVar)
    _, E = intersection_env(Tuple{Dict{Int,Type}, Type, Any},
                            Tuple{Dict{K,V}, Any, Int} where {K,V})
    @test E[2] == Type

    # issue #20611
    I, E = intersection_env(Tuple{Ref{Integer},Int,Any}, Tuple{Ref{Z},Z,Z} where Z)
    @test isequal_type(I, Tuple{Ref{Integer},Int,Integer})
    @test E[1] == Integer

    # issue #21118
    A = Tuple{Ref, Vararg{Any}}
    B = Tuple{Vararg{Union{Z,Ref,Nothing}}} where Z<:Union{Ref,Nothing}
    @test B <: _type_intersect(A, B)
    # TODO: this would be a better version of that test:
    #let T = _type_intersect(A, B)
    #    @test T <: A
    #    @test T <: B
    #    @test Tuple{Ref, Vararg{Union{Ref,Nothing}}} <: T
    #end
    @testintersect(Tuple{Int,Any,Vararg{A}} where A>:Integer,
                   Tuple{Any,Int,Vararg{A}} where A>:Integer,
                   Tuple{Int,Int,Vararg{A}} where A>:Integer)

    # issue #21132
    @testintersect(Pair{L,Tuple{L,Pair{L,HL}}} where {L,HL},
                   Pair{R,Tuple{Pair{R,HR},R}} where {R,HR},
                   Bottom)  # X == Pair{X,...} is not satisfiable

    # issue #20671 --- this just took too long
    @testintersect(Tuple{Type{SIQ20671{T, mT, kgT, sT, AT, KT, molT, cdT, radT, srT}},
                         SIQ20671{S, mS, kgS, sS, AS, KS, molS, cdS, radS, srS}} where {T, mT, kgT, sT, AT, KT, molT, cdT, radT, srT,
                                                                                        S, mS, kgS, sS, AS, KS, molS, cdS, radS, srS},
                   Tuple{Type{T}, T} where T,
                   Tuple{Type{SIQ20671{T,mS,kgS,sS,AS,KS,molS,cdS,radS,srS}},
                         SIQ20671{T,mS,kgS,sS,AS,KS,molS,cdS,radS,srS}} where {T,mS,kgS,sS,AS,KS,molS,cdS,radS,srS})

    # issue #21243
    @testintersect(Tuple{Ref{Ref{T}} where T, Ref},
                   Tuple{Ref{T}, Ref{T}} where T,
                   Tuple{Ref{Ref{T}}, Ref{Ref{T}}} where T)
    # issue #29208
    @testintersect(Tuple{Ref{Ref{T}} where T, Ref{Ref{Int}}},
                   Tuple{Ref{T}, Ref{T}} where T,
                   Tuple{Ref{Ref{Int}}, Ref{Ref{Int}}})
    @testintersect(Tuple{Vector{Pair{K,V}}, Vector{Pair{K,V}}} where K where V,
                   Tuple{(Array{Pair{Ref{_2},_1},1} where _2 where _1),
                         Array{Pair{Ref{Int64},Rational{Int64}},1}},
                   Tuple{Vector{Pair{Ref{Int64},Rational{Int64}}},
                         Vector{Pair{Ref{Int64},Rational{Int64}}}})
    @testintersect(Vector{>:Missing}, Vector{Int}, Union{})

    # issue #23685
    @testintersect(Pair{Type{Z},Z} where Z,
                   Pair{Type{Ref{T}} where T, Ref{Float64}},
                   Bottom)
    @testintersect(Tuple{Type{Z},Z} where Z,
                   Tuple{Type{Ref{T}} where T, Ref{Float64}},
                   !Bottom)
    @test_broken typeintersect(Tuple{Type{Z},Z} where Z,
                               Tuple{Type{Ref{T}} where T, Ref{Float64}}) ==
        Tuple{Type{Ref{Float64}},Ref{Float64}}

    # issue #32607
    @testintersect(Type{<:Tuple{Integer,Integer}},
                   Type{Tuple{Int,T}} where T,
                   Type{Tuple{Int,T}} where T<:Integer)
    @testintersect(Type{<:Tuple{Any,Vararg{Any}}},
                   Type{Tuple{Vararg{Int,N}}} where N,
                   Type{Tuple{Int,Vararg{Int,N}}} where N)

    @testintersect(Type{<:Array},
                   Type{AbstractArray{T}} where T,
                   Bottom)

    @testintersect(Tuple{Type{Tuple{Vararg{Integer}}}, Tuple},
                   Tuple{Type{Tuple{Vararg{V}}}, Tuple{Vararg{V}}} where {V},
                   Tuple{Type{Tuple{Vararg{Integer}}},Tuple{Vararg{Integer}}})
    @testintersect(Tuple{Type{Tuple{Vararg{Union{Int,Symbol}}}}, Tuple},
                   Tuple{Type{Tuple{Vararg{V}}}, Tuple{Vararg{V}}} where {V},
                   Tuple{Type{Tuple{Vararg{Union{Int,Symbol}}}},Tuple{Vararg{Union{Int,Symbol}}}})

    # non types
    @testintersect(Tuple{1}, Tuple{Any}, Tuple{1})

    # tests for robustness after incorrect datatype allocation normalization
    @test typeintersect(Vector{Tuple{T, T} where Number<:T<:Number}, Vector{Tuple{Number, Number}}) === Vector{Tuple{T, T} where Number<:T<:Number}
    @test typeintersect(Vector{Tuple{Number, Number}}, Vector{Tuple{T, T} where Number<:T<:Number}) === Vector{Tuple{Number, Number}}
end

function test_intersection_properties()
    for i in eachindex(menagerie)
        T = menagerie[i]
        for j in eachindex(menagerie)
            S = menagerie[j]
            I = _type_intersect(T,S)
            I2 = _type_intersect(S,T)
            @test isequal_type(I, I2)
            if i > length(easy_menagerie) || j > length(easy_menagerie)
                # @test issub(I, T) || issub(I, S)
            else
                @test issub(I, T) && issub(I, S)
            end
            if issub(T, S)
                @test isequal_type(I, T)
            end
        end
    end
end

test_1()
test_2()
test_diagonal()
test_3()
test_4()
test_5()
test_6()
test_7()
test_Type()
test_old()
test_intersection()
test_properties()
test_intersection_properties()


let S = ccall(:jl_new_structv, Any, (Any, Ptr{Cvoid}, UInt32), UnionAll, [TypeVar(:T), Any], 2),
    VS = TypeVar(:T),
    T = ccall(:jl_new_structv, Any, (Any, Ptr{Cvoid}, UInt32), UnionAll, [VS, VS], 2)
    # check (T where T) == (Any where T)
    # these types are not normally valid, but check them just to make sure subtyping is robust
    @test T <: S
    @test S <: T
end

# issue #20121
@test NTuple{170,Matrix{Int}} <: (Tuple{Vararg{Union{Array{T,1},Array{T,2},Array{T,3}}}} where T)

# Issue #12580
abstract type AbstractMyType12580{T} end
struct MyType12580{T}<:AbstractMyType12580{T} end
tpara(::Type{A}) where {A<:AbstractMyType12580} = tpara(supertype(A))
tpara(::Type{AbstractMyType12580{I}}) where {I} = I
@test tpara(MyType12580{true})

# Issue #18348
f18348(::Type{T}, x) where {T<:Any} = 1
f18348(::Type{T}, x::T) where {T<:Any} = 2
@test length(methods(f18348, Tuple{Type{Any},Any})) == 1

# Issue #13165
@test Symmetric{Float64,Matrix{Float64}} <: LinearAlgebra.RealHermSymComplexHerm
@test Hermitian{Float64,Matrix{Float64}} <: LinearAlgebra.RealHermSymComplexHerm
@test Hermitian{ComplexF64,Matrix{ComplexF64}} <: LinearAlgebra.RealHermSymComplexHerm

# Issue #12721
f12721(::T) where {T<:Type{Int}} = true
@test_throws MethodError f12721(Float64)

# implicit "covariant" type parameters:
mutable struct TwoParams{S,T}; x::S; y::T; end
@test TwoParams{<:Real,<:Number} == (TwoParams{S,T} where S<:Real where T<:Number) ==
      (TwoParams{S,<:Number} where S<:Real) == (TwoParams{<:Real,T} where T<:Number)
@test TwoParams(3,0im) isa TwoParams{<:Real,<:Number}
@test TwoParams(3,"foo") isa TwoParams{<:Real}
@test !(TwoParams(3im,0im) isa TwoParams{<:Real,<:Number})
@test !(TwoParams(3,"foo") isa TwoParams{<:Real,<:Number})
ftwoparams(::TwoParams) = 1
ftwoparams(::TwoParams{<:Real}) = 2
ftwoparams(::TwoParams{<:Real,<:Real}) = 3
@test ftwoparams(TwoParams('x',3)) == 1
@test ftwoparams(TwoParams(3,'x')) == 2
@test ftwoparams(TwoParams(3,4)) == 3
@test !([TwoParams(3,4)] isa Vector{TwoParams{<:Real,<:Real}})
@test TwoParams{<:Real,<:Real}[TwoParams(3,4)] isa Vector{TwoParams{<:Real,<:Real}}
@test [TwoParams(3,4)] isa Vector{<:TwoParams{<:Real,<:Real}}
@test [TwoParams(3,4)] isa (Vector{TwoParams{T,T}} where T<:Real)

# implicit "contravariant" type parameters:
@test TwoParams{>:Int,<:Number} == (TwoParams{S,T} where S>:Int where T<:Number) ==
      (TwoParams{S,<:Number} where S>:Int) == (TwoParams{>:Int,T} where T<:Number)
@test TwoParams(3,0im) isa TwoParams{>:Int,<:Number}
@test TwoParams{Real,Complex}(3,0im) isa TwoParams{>:Int,<:Number}
@test !(TwoParams(3.0,0im) isa TwoParams{>:Int,<:Number})
@test !(TwoParams(3,'x') isa TwoParams{>:Int,<:Number})

# supertype operator
@test !(Int >: Integer)
@test Integer >: Int

# tolerate non-types in Tuples
@test typeintersect(Tuple{0}, Tuple{T} where T) === Tuple{0}

# TypeVars deduced as non-type constants (#20869)
@testintersect(Tuple{Val{0}, Val{Val{N}}} where N, Tuple{Val{N}, Val{Val{N}}} where N, Tuple{Val{0},Val{Val{0}}})
@testintersect(Tuple{Val{N}, Val{Val{0}}} where N, Tuple{Val{N}, Val{Val{N}}} where N, Tuple{Val{0},Val{Val{0}}})

@testintersect(Tuple{Val{Val{0}}, Val{N}} where N, Tuple{Val{Val{N}}, Val{N}} where N, Tuple{Val{Val{0}},Val{0}})
@testintersect(Tuple{Val{Val{N}}, Val{0}} where N, Tuple{Val{Val{N}}, Val{N}} where N, Tuple{Val{Val{0}},Val{0}})

# a bunch of cases found by fuzzing
let a = Tuple{Float64,T7} where T7,
    b = Tuple{S5,Tuple{S5}} where S5
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test I1 <: I2
    @test I2 <: I1
    @test I1 <: a
    @test I2 <: a
    @test I1 <: b
    @test I2 <: b
end
let a = Tuple{T1,T1} where T1,
    b = Tuple{Val{S2},S6} where S2 where S6
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test I1 <: I2
    @test I2 <: I1
    @test I1 <: a
    @test I2 <: a
    @test I1 <: b
    @test I2 <: b
end
let a = Val{Tuple{T1,T1}} where T1,
    b = Val{Tuple{Val{S2},S6}} where S2 where S6
    @testintersect(a, b, Val{Tuple{Val{T},Val{T}}} where T)
end
let a = Tuple{Float64,T3,T4} where T4 where T3,
    b = Tuple{S2,Tuple{S3},S3} where S2 where S3
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test_broken I1 <: I2
    @test I2 <: I1
    @test I1 <: a
    @test I2 <: a
    @test_broken I1 <: b
    @test I2 <: b
end
let a = Tuple{T1,Tuple{T1}} where T1,
    b = Tuple{Float64,S3} where S3
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test I1 <: I2
    @test I2 <: I1
    @test I1 <: a
    @test I2 <: a
    @test I1 <: b
    @test I2 <: b
end
let a = Tuple{5,T4,T5} where T4 where T5,
    b = Tuple{S2,S3,Tuple{S3}} where S2 where S3
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test_broken I1 <: I2
    @test I2 <: I1
    @test I1 <: a
    @test I2 <: a
    @test_broken I1 <: b
    @test I2 <: b
end
let a = Tuple{T2,Tuple{T4,T2}} where T4 where T2,
    b = Tuple{Float64,Tuple{Tuple{S3},S3}} where S3
    @test typeintersect(a, b) <: b
end
let a = Tuple{Tuple{T2,4},T6} where T2 where T6,
    b = Tuple{Tuple{S2,S3},Tuple{S2}} where S2 where S3
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test_broken I1 <: I2
    @test I2 <: I1
    @test I1 <: a
    @test I2 <: a
    @test_broken I1 <: b
    @test I2 <: b
end
let a = Tuple{T3,Int64,Tuple{T3}} where T3,
    b = Tuple{S3,S3,S4} where S4 where S3
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test I1 <: I2
    @test I2 <: I1
    @test_broken I1 <: a
    @test I2 <: a
    @test I1 <: b
    @test I2 <: b
end
let a = Tuple{T1,Val{T2},T2} where T2 where T1,
    b = Tuple{Float64,S1,S2} where S2 where S1
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test I1 <: I2
    @test I2 <: I1
    @test_broken I1 <: a
    @test_broken I2 <: a
    @test I1 <: b
    @test I2 <: b
end
let a = Tuple{T1,Val{T2},T2} where T2 where T1,
    b = Tuple{Float64,S1,S2} where S2 where S1
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test I1 <: I2
    @test I2 <: I1
    @test_broken I1 <: a
    @test_broken I2 <: a
    @test I1 <: b
    @test I2 <: b
end
let a = Tuple{Float64,T1} where T1,
    b = Tuple{S1,Tuple{S1}} where S1
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test I1 <: I2
    @test I2 <: I1
    @test I1 <: a
    @test I2 <: a
    @test I1 <: b
    @test I2 <: b
end
let a = Tuple{Val{T1},T2,T2} where T2 where T1,
    b = Tuple{Val{Tuple{S2}},S3,Float64} where S2 where S3
    @testintersect(a, b, Tuple{Val{Tuple{S2}},Float64,Float64} where S2)
end
let a = Tuple{T1,T2,T2} where T1 where T2,
    b = Tuple{Val{S2},S2,Float64} where S2,
    x = Tuple{Val{Float64},Float64,Float64}
    I1 = typeintersect(a, b)
    I2 = typeintersect(b, a)
    @test x <: I1
    @test x <: I2
    @test I1 <: I2
    @test I2 <: I1
    @test I1 <: a
    @test I2 <: a
    @test_broken I1 <: b
    @test_broken I2 <: b
end
@testintersect(Val{Tuple{T1,Val{T2},Val{Int64},Tuple{Tuple{T3,5,Float64},T4,T2,T5}}} where T1 where T5 where T4 where T3 where T2,
               Val{Tuple{Tuple{S1,5,Float64},Val{S2},S3,Tuple{Tuple{Val{Float64},5,Float64},2,Float64,S4}}} where S2 where S3 where S1 where S4,
               Val{Tuple{Tuple{S1, 5, Float64}, Val{Float64}, Val{Int64}, Tuple{Tuple{Val{Float64}, 5, Float64}, 2, Float64, T5}}} where {T5, S1})

# issue #20992
abstract type A20992{T,D,d} end
abstract type B20992{SV,T,D,d} <: A20992{T,D,d} end
struct C20992{S,n,T,D,d} <: B20992{NTuple{n,S},T,D,d}
end
@testintersect(Tuple{A20992{R, D, d} where d where D, Int} where R,
               Tuple{C20992{S, n, T, D, d} where d where D where T where n where S, Any},
               Tuple{C20992, Int})

# Issue #19414
let ex = try struct A19414 <: Base.AbstractSet end catch e; e end
    @test isa(ex, ErrorException) && ex.msg == "invalid subtyping in definition of A19414: can only subtype data types."
end

# issue #20103, OP and comments
struct TT20103{X,Y} end
f20103(::Type{TT20103{X,Y}},x::X,y::Y) where {X,Y} = 1
f20103(::Type{TT20103{X,X}},x::X) where {X} = 100
@testintersect(Type{NTuple{N,E}} where E where N, Type{NTuple{N,E} where N} where E, Union{})
let ints = (Int, Int32, UInt, UInt32)
    Ints = Union{ints...}
    vecs = []
    for i = 2:4, t in ints
        push!(vecs, NTuple{i, t})
    end
    Vecs = Union{vecs...}
    T = Type{Tuple{V, I}} where V <: Vecs where I <: Ints
    @testintersect(T, T, T)
    test(a::Type{Tuple{V, I}}) where {V <: Vecs, I <: Ints} = I
end

# issue #21191
let T1 = Val{Val{Val{Union{Int8,Int16,Int32,Int64,UInt8,UInt16}}}},
    T2 = Val{Val{Val{Union{Int8,Int16,Int32,Int64,UInt8, S}}}} where S
    @test T1 <: T2
end

# issue #21613
abstract type A21613{S <: Tuple} end
struct B21613{S <: Tuple, L} <: A21613{S}
    data::NTuple{L,Float64}
end
@testintersect(Tuple{Type{B21613{Tuple{L},L}} where L, Any},
               Tuple{Type{SA}, Tuple} where SA<:(A21613{S} where S<:Tuple),
               Tuple{Type{B21613{Tuple{L},L}} where L, Tuple})

# issue #22239
@testintersect(Val{Pair{T,T}} where T,
               Val{Pair{Int,T}} where T,
               Val{Pair{Int,Int}})

# issue #23024
@testintersect(Tuple{DataType, Any},
               Tuple{Type{T}, Int} where T,
               Tuple{DataType, Int})

# issue #23430
@test [0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.;
       0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.;
       0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.;
       0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.;
       0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.; 0 0.;
       0 0.; 0 0.; 0 0.; 0 0.] isa Matrix{Float64}
@test !(Tuple{Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64,
              Int64,Float64,Int64,Float64,Int64,Float64,Int64,Float64} <: (Tuple{Vararg{T}} where T<:Number))

# part of issue #23327
let
    triangular(::Type{<:AbstractArray{T}}) where {T} = T
    triangular(::Type{<:AbstractArray}) = Any
    @test triangular(Array{Array{T, 1}, 1} where T) === Any
end

# issue #23908
@test Array{Union{Int128, Int16, Int32, Int8}, 1} <: Array{Union{Int128, Int32, Int8, _1}, 1} where _1
let A = Pair{Nothing, Pair{Array{Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8}, 1}, Nothing}},
    B = Pair{Nothing, Pair{Array{Union{Int8, UInt128, UInt16, UInt32, UInt64, UInt8, _1}, 1}, Nothing}} where _1
    @test A <: B
    @test !(B <: A)
end

# issue #22688
let X = Ref{Tuple{Array{Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8}, 1}}}
    @test !(X <: Ref{Tuple{Array{Union{Int8, UInt128, UInt16, UInt32, UInt64, UInt8, S}}}} where S)
    @test X <: Ref{Tuple{Array{Union{Int8, UInt128, UInt16, UInt32, UInt64, UInt8, S}, 1}}} where S
end
let X = Ref{Tuple{Array{Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8}, 1}, Array{Union{Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8}, 1}}},
    Y = Ref{Tuple{Array{Union{Int8, UInt128, UInt16, UInt32, UInt64, UInt8, S}, 1}, Array{Union{Int8, UInt128, UInt16, UInt32, UInt64, UInt8, T}, 1}}} where S where T
    @test X <: Y
end

# issue #23764
@test Tuple{Ref{Union{T,Int}} where {T}} <: Tuple{Ref{T}} where {T}
@test Tuple{Ref{Union{T,Int}} where {T}} <: Tuple{Ref{T} where {T}}
@test (Tuple{Ref{Union{T,Int}}} where T) <: Tuple{Ref{T}} where {T}
@test (Tuple{Ref{Union{T,Int}}} where T) <: Tuple{Ref{T} where {T}}
@test Tuple{Tuple{Tuple{R}} where R} <: Tuple{Tuple{S}} where S
struct A23764{T, N, S} <: AbstractArray{Union{T, S}, N}; end
@test Tuple{A23764{Int, 1, T} where T} <: Tuple{AbstractArray{T,N}} where {T,N}
struct A23764_2{T, N, S} <: AbstractArray{Union{Ref{T}, S}, N}; end
@test Tuple{A23764_2{T, 1, Nothing} where T} <: Tuple{AbstractArray{T,N}} where {T,N}
@test Tuple{A23764_2{T, 1, Nothing} where T} <: Tuple{AbstractArray{T,N} where {T,N}}

# issue #50716
@test !<:(Ref{Vector{Tuple{K}} where K}, Ref{<:Vector{K}} where K)

# issue #26131
@test !(Vector{Vector{Number}} <: Vector{Union{Vector{Number}, Vector{S}}} where S<:Integer)

# issue #24305
f24305(x) = [g24305(x) g24305(x) g24305(x) g24305(x); g24305(x) g24305(x) 0 0];
@test_throws UndefVarError f24305(1)

f1_24305(x,y,z) = x*y-z^2-1
f2_24305(x,y,z) = x*y*z+y^2-x^2-2
f3_24305(x,y,z) = exp(x)+z-exp(y)-3
Fun_24305(x) = [ f1_24305(x[1],x[2],x[3]); f2_24305(x[1],x[2],x[3]); f3_24305(x[1],x[2],x[3]) ]
Jac_24305(x) = [ x[2] x[1] -2*x[3] ; x[2]*x[3]-2x[1]  x[1]*x[3]+2x[2]  x[1]*x[2] ; exp(x[1])  -exp(x[2])  1 ]

x_24305 = fill(1.,3)

for it = 1:5
    h = - \(Jac_24305(x_24305), Fun_24305(x_24305))
    global x_24305 = x_24305 + h
end

@test round.(x_24305, digits=2) == [1.78, 1.42, 1.24]

# PR #24399
let (t, e) = intersection_env(Tuple{Union{Int,Int8}}, Tuple{T} where T)
    @test e[1] isa TypeVar
end

# issue #25430
@test Vector{Tuple{Any}}() isa Vector{Tuple{>:Int}}
@test Vector{Tuple{>:Int}}() isa Vector{Tuple{Any}}
@test Vector{Tuple{Any}} == Vector{Tuple{>:Int}}
@test Vector{Vector{Tuple{Any}}} == Vector{Vector{Tuple{>:Int}}}
f25430(t::Vector{Tuple{Any}}) = true
g25430(t::Vector{Tuple{>:Int}}) = true
@test f25430(Vector{Tuple{>:Int}}())
@test g25430(Vector{Tuple{Any}}())
@testintersect(Vector{Tuple{>:Int}}, Vector{Tuple{Any}}, Vector{Tuple{Any}})
@testintersect(Vector{Vector{Tuple{>:Int}}}, Vector{Vector{Tuple{Any}}}, Vector{Vector{Tuple{Any}}})

# issue #24521
g24521(::T, ::T) where {T} = T
@test_throws MethodError g24521(Tuple{Any}, Tuple{T} where T)
@test g24521(Vector, Matrix) == UnionAll
@test [Tuple{Vararg{Int64}}, Tuple{Vararg{Int64,N}} where N] isa Vector{Type}
f24521(::Type{T}, ::Type{T}) where {T} = T
@test f24521(Tuple{Any}, Tuple{T} where T) == Tuple{Any}
@test f24521(Tuple{Vararg{Int64}}, Tuple{Vararg{Int64,N}} where N) == Tuple{Vararg{Int64,N}} where N

# issue #26654
@test !(Ref{Union{Int64, Ref{Number}}} <: Ref{Union{Ref{T}, T}} where T)
@test !(Ref{Union{Int64, Val{Number}}} <: Ref{Union{Val{T}, T}} where T)
@test !(Ref{Union{Ref{Number}, Int64}} <: Ref{Union{Ref{T}, T}} where T)
@test !(Ref{Union{Val{Number}, Int64}} <: Ref{Union{Val{T}, T}} where T)
@test !(Val{Ref{Union{Int64, Ref{Number}}}} <: Val{S} where {S<:Ref{Union{Ref{T}, T}} where T})
@test !(Tuple{Ref{Union{Int64, Ref{Number}}}} <: Tuple{S} where {S<:Ref{Union{Ref{T}, T}} where T})

# issue #26180
@test !(Ref{Union{Ref{Int64}, Ref{Number}}} <: Ref{Ref{T}} where T)
@test !(Ref{Union{Ref{Int64}, Ref{Number}}} <: Ref{Union{Ref{T}, Ref{T}}} where T)

# issue #25240, #26405
f26405(::Type{T}) where {T<:Union{Integer, Missing}} = T
@test f26405(Union{Missing, Int}) == Union{Missing, Int}

# issue #24748
abstract type Foo24748{T1,T2,T3} end
@test !(Foo24748{Int,Float64,Ref{Integer}} <: Foo24748{<:T,<:T,Ref{T}} where T)

# issue #26129
@test !(Tuple{Type{Union{Missing, Float64}}, Type{Vector{Missing}}} <: Tuple{Type{T}, Type{Vector{T}}} where T)
@test !(Tuple{Type{Union{Missing, Float64}}, Type{Vector{Float64}}} <: Tuple{Type{T}, Type{Vector{T}}} where T)
@test [[1],[missing]] isa Vector{Vector}
@test [[missing],[1]] isa Vector{Vector}

# issue #26453
@test (Tuple{A,A,Number} where A>:Number) <: Tuple{T,T,S} where T>:S where S
@test (Tuple{T,T} where {S,T>:S}) == (Tuple{T,T} where {S,T>:S})
f26453(x::T,y::T) where {S,T>:S} = 0
@test f26453(1,2) == 0
@test f26453(1,"") == 0
g26453(x::T,y::T) where {S,T>:S} = T
@test_throws UndefVarError(:T) g26453(1,1)
@test issub_strict((Tuple{T,T} where T), (Tuple{T,T} where {S,T>:S}))

# issue #27632
@test !(Tuple{Array{Int,0}, Int, Vararg{Int}} <: Tuple{AbstractArray{T,N}, Vararg{Int,N}} where {T, N})
@test !(Tuple{Array{Int,0}, Int, Vararg{Int}} <: Tuple{AbstractArray{T,N}, Vararg{Any,N}} where {T, N})
@test !(Tuple{Array{Int,0}, Vararg{Any}} <: Tuple{AbstractArray{T,N}, Vararg{Any,N}} where {T, N})
@test Tuple{Array{Int,0},} <: Tuple{AbstractArray{T,N}, Vararg{Any,N}} where {T, N}
@test !(Tuple{Array{Int,0}, Any} <: Tuple{AbstractArray{T,N}, Vararg{Any,N}} where {T, N})

# issue #26827
@test typeintersect(Union{Int8,Int16,Int32}, Union{Int8,Int16,Int64}) == Union{Int8, Int16}
@test typeintersect(Union{Int8,Int16,Float64}, Integer) == Union{Int8, Int16}
@test typeintersect(Integer, Union{Int8,Int16,Float64}) == Union{Int8, Int16}
@test typeintersect(Tuple{Ref{Int},Any},
                    Tuple{Ref{T},Union{Val{N}, Array{Float32,N}}} where {T,N}) ==
                    Tuple{Ref{Int},Union{Val{N}, Array{Float32,N}}} where N
@test typeintersect(Tuple{Ref{T},Union{Val{N}, Array{Float32,N}}} where {T,N},
                    Tuple{Ref{Int},Any}) ==
                    Tuple{Ref{Int},Union{Val{N}, Array{Float32,N}}} where N

# issue #28256
@test Pair{(:a,), Pair{(:a,),Tuple{Int}}} isa Type{Pair{names,T}} where {names, T<:Pair{names,<:Tuple}}
@test Type{Pair{(:a,), Pair{(:a,),Tuple{Int}}}} <: Type{Pair{names,T}} where {names, T<:Pair{names,<:Tuple}}
struct A28256{names, T<:NamedTuple{names, <:Tuple}}
    x::T
end
@test A28256{(:a,), NamedTuple{(:a,),Tuple{Int}}}((a=1,)) isa A28256

# issue #29468
@testintersect(Tuple{Vararg{Val{N}, N}} where N,
               Tuple{Val{2}, Vararg{Val{2}}},
               Tuple{Val{2}, Val{2}})
@testintersect(Tuple{Vararg{Val{N}, N}} where N,
               Tuple{Val{3}, Vararg{Val{3}}},
               Tuple{Val{3}, Val{3}, Val{3}})
@testintersect(Tuple{Vararg{Val{N}, N}} where N,
               Tuple{Val{1}, Vararg{Val{2}}},
               Tuple{Val{1}})
@testintersect(Tuple{Vararg{Val{N}, N}} where N,
               Tuple{Val{2}, Vararg{Val{3}}},
               Union{})

# issue #25752
@testintersect(Base.RefValue, Ref{Union{Int,T}} where T,
               Base.RefValue{Union{Int,T}} where T)
# issue #29269
@testintersect((Tuple{Int, Array{T}} where T),
               (Tuple{Any, Vector{Union{Missing,T}}} where T),
               (Tuple{Int, Vector{Union{Missing,T}}} where T))
@testintersect((Tuple{Int, Array{T}} where T),
               (Tuple{Any, Vector{Union{Missing,Nothing,T}}} where T),
               (Tuple{Int, Vector{Union{Missing,Nothing,T}}} where T))
# issue #32582
let A = Tuple{Any, Type{Union{Nothing, Int64}}},
    B = Tuple{T, Type{Union{Nothing, T}}} where T,
    I = typeintersect(A, B),
    J = typeintersect(B, A)
    # TODO: improve precision
    @test I >: Tuple{Int64,Type{Union{Nothing, Int64}}}
    @test J >: Tuple{Int64,Type{Union{Nothing, Int64}}}
end
@testintersect(Union{Array{T,1},Array{T,2}} where T<:Union{Float32,Float64},
               Union{AbstractMatrix{Float32},AbstractVector{Float32}},
               Union{Array{Float32,2}, Array{Float32,1}})
let A = Tuple{Type{Union{Missing,T}},Any} where T,
    B = Tuple{Type{Union{Nothing,T}},Any} where T
    I = typeintersect(A, B)
    J = typeintersect(B, A)
    @test I >: Tuple{Type{Union{Nothing,Missing,T}}, Any} where T
    @test J >: Tuple{Type{Union{Nothing,Missing,T}}, Any} where T
end

# issue #29955
struct M29955{T, TV<:AbstractVector{T}}
end
@testintersect(M29955,
               M29955{<:Any,TV} where TV>:Vector{Float64},
               M29955{Float64,TV} where Array{Float64,1}<:TV<:AbstractArray{Float64,1})

struct A29955{T, TV<:AbstractVector{T}, TModel<:M29955{T,TV}}
end
@testintersect(Tuple{Type{A29955{Float64,Array{Float64,1},_1}} where _1,
                     Any},
               Tuple{Type{A29955{T,TV,TM}},
                     TM} where {T,TV<:AbstractVector{T},TM<:M29955{T,TV}},
               Tuple{Type{A29955{Float64,Array{Float64,1},TM}},
                   M29955{Float64,Vector{Float64}}} where TM<:M29955{Float64,Array{Float64,1}})
let M = M29955{T,Vector{Float64}} where T
    @test M == (M29955{T,Vector{Float64}} where T)
    @test M{Float64} == M29955{Float64,Vector{Float64}}
    @test_throws TypeError M{Float32}
    @test_throws TypeError M{Real}
end

# issue #30122
@testintersect(Tuple{Pair{Int64,2}, NTuple},
               Tuple{Pair{F,N},Tuple{Vararg{F,N}}} where N where F,
               Tuple{Pair{Int64,2}, Tuple{Int64,Int64}})

# issue #30335
@testintersect(Tuple{Any,Rational{Int},Int},
               Tuple{LT,R,I} where LT<:Union{I, R} where R<:Rational{I} where I<:Integer,
               Tuple{LT,Rational{Int},Int} where LT<:Union{Rational{Int},Int})

@testintersect(Tuple{Any,Tuple{Int},Int},
               Tuple{LT,R,I} where LT<:Union{I, R} where R<:Tuple{I} where I<:Integer,
               Tuple{LT,Tuple{Int},Int} where LT<:Union{Tuple{Int},Int})
# fails due to this:
let U = Tuple{Union{LT, LT1},Union{R, R1},Int} where LT1<:R1 where R1<:Tuple{Int} where LT<:Int where R<:Tuple{Int},
    U2 = Union{Tuple{LT,R,Int} where LT<:Int where R<:Tuple{Int}, Tuple{LT,R,Int} where LT<:R where R<:Tuple{Int}},
    V = Tuple{Union{Tuple{Int},Int},Tuple{Int},Int},
    V2 = Tuple{L,Tuple{Int},Int} where L<:Union{Tuple{Int},Int}
    @test U == U2
    @test U == V
    @test U == V2
    @test V == V2
    @test U2 == V
    @test_broken U2 == V2
end

# issue #31082 and #30741
@test typeintersect(Tuple{T, Ref{T}, T} where T,
                    Tuple{Ref{S}, S, S} where S) != Union{}
# TODO: improve this bound
@testintersect(Tuple{Pair{B,C},Union{C,Pair{B,C}},Union{B,Real}} where {B,C},
               Tuple{Pair{B,C},C,C} where {B,C},
               Tuple{Pair{B,C}, Union{Pair{B,C},C},Union{Real,B}} where {B,C})
f31082(::Pair{B, C}, ::Union{C, Pair{B, C}}, ::Union{B, Real}) where {B, C} = 0
f31082(::Pair{B, C}, ::C, ::C) where {B, C} = 1
@test f31082(""=>1, 2, 3) == 1
@test f31082(""=>1, 2, "") == 0
@test f31082(""=>1, 2, 3.0) == 0
@test f31082(Pair{Any,Any}(1,2), 1, 2) == 1
@test f31082(Pair{Any,Any}(1,2), Pair{Any,Any}(1,2), 2) == 1
@test f31082(Pair{Any,Any}(1,2), 1=>2, 2.0) == 1

# issue #31115
@testintersect(Tuple{Ref{Z} where Z<:(Ref{Y} where Y<:Tuple{<:B}), Int} where B,
               Tuple{Ref{Z} where Z<:(Ref{Y} where Y<:Tuple{  B}), Any} where B<:AbstractMatrix,
               Tuple{Ref{Z} where Z<:(Ref{Y} where Y<:Tuple{  B}), Int} where B<:AbstractMatrix)

# issue #31190
@test (Tuple{T} where T <: Union{Int,Bool}) <: Union{Tuple{Int}, Tuple{Bool}}

# issue #31439
@testintersect(Tuple{Type{Val{T}},Integer,T} where T,
               Tuple{Type,Int,Any},
               Tuple{Type{Val{T}},Int,T} where T)
@testintersect(Tuple{Type{Val{T}},Integer,T} where T,
               Tuple{Type,Int,Integer},
               Tuple{Type{Val{T}},Int,Integer} where T)
@testintersect(Tuple{Type{Val{T}},Integer,T} where T>:Integer,
               Tuple{Type,Int,Integer},
               Tuple{Type{Val{T}},Int,Integer} where T>:Integer)
@testintersect(Tuple{Type{Val{T}},Integer,T} where T>:Integer,
               Tuple{Type,Int,Int},
               Tuple{Type{Val{T}},Int,Int} where T>:Integer)

# issue #31496
CovType{T} = Union{AbstractArray{T,2},
                   Vector{UpperTriangular{T,Matrix{T}}}}
@testintersect(Pair{<:Any, <:AbstractMatrix},
               Pair{T,     <:CovType{T}} where T<:AbstractFloat,
               Pair{T,S} where S<:AbstractArray{T,2} where T<:AbstractFloat)

# issue #31703
@testintersect(Pair{<:Any, Ref{Tuple{Ref{Ref{Tuple{Int}}},Ref{Float64}}}},
               Pair{T, S} where S<:(Ref{A} where A<:(Tuple{C,Ref{T}} where C<:(Ref{D} where D<:(Ref{E} where E<:Tuple{FF}) where FF<:B)) where B) where T,
               Pair{Float64, Ref{Tuple{Ref{Ref{Tuple{Int}}},Ref{Float64}}}})

module I31703
using Test, LinearAlgebra
import Base: OneTo, Slice

struct BandedMatrix{T, CONTAINER, RAXIS} end
struct Ones{T, N, Axes} end
const RestrictionMatrix = BandedMatrix{<:Int, <:Ones}
struct Applied{Style, Args<:Tuple} end
const Mul = Applied{Style,Factors} where Factors<:Tuple where Style
const RestrictedBasis{B} = Mul{<:Any,<:Tuple{B, <:RestrictionMatrix}}
struct ApplyQuasiArray{T, N, App<:Applied} end
const MulQuasiArray = ApplyQuasiArray{T,N,MUL} where MUL<:(Applied{Style,Factors} where Factors<:Tuple where Style) where N where T
const RestrictedQuasiArray{T,N,B} = MulQuasiArray{T,N,<:RestrictedBasis{B}}
const BasisOrRestricted{B} = Union{B,RestrictedBasis{<:B},<:RestrictedQuasiArray{<:Any,<:Any,<:B}}
struct QuasiAdjoint{T,S} end
const AdjointRestrictedBasis{B} = Mul{<:Any,<:Tuple{<:Adjoint{<:Any,<:RestrictionMatrix}, <:QuasiAdjoint{<:Any,B}}}
const AdjointRestrictedQuasiArray{T,N,B} = MulQuasiArray{T,N,<:AdjointRestrictedBasis{B}}
const AdjointBasisOrRestricted{B} = Union{<:QuasiAdjoint{<:Any,B},AdjointRestrictedBasis{<:B},<:AdjointRestrictedQuasiArray{<:Any,<:Any,<:B}}
const RadialOperator{T,B,M<:AbstractMatrix{T}} = Mul{<:Any,<:Tuple{<:BasisOrRestricted{B},M,<:AdjointBasisOrRestricted{B}}}
const HFPotentialOperator{T,B} = RadialOperator{T,B,Diagonal{T,Vector{T}}}
struct HFPotential{kind,T,B,RO<:HFPotentialOperator{T,B},P<:Integer} end

T = HFPotential{_A,Float64,Any,Applied{Int,Tuple{ApplyQuasiArray{Float64,2,Applied{Int,Tuple{Any,BandedMatrix{Int,Ones{Int,2,Tuple{OneTo{Int},OneTo{Int}}},OneTo{Int}}}}},Diagonal{Float64,Array{Float64,1}},ApplyQuasiArray{Float64,2,Applied{Int,Tuple{Adjoint{Int,BandedMatrix{Int,Ones{Int,2,Tuple{OneTo{Int},OneTo{Int}}},OneTo{Int}}},QuasiAdjoint{Float64,Any}}}}}},_B} where _B where _A

let A = typeintersect(HFPotential, T),
    B = typeintersect(T, HFPotential)
    @test A == B == HFPotential{kind,Float64,Any,Applied{Int,Tuple{ApplyQuasiArray{Float64,2,Applied{Int,Tuple{Any,BandedMatrix{Int,Ones{Int,2,Tuple{OneTo{Int},OneTo{Int}}},OneTo{Int}}}}},Diagonal{Float64,Array{Float64,1}},ApplyQuasiArray{Float64,2,Applied{Int,Tuple{Adjoint{Int,BandedMatrix{Int,Ones{Int,2,Tuple{OneTo{Int},OneTo{Int}}},OneTo{Int}}},QuasiAdjoint{Float64,Any}}}}}},P} where P<:Integer where kind
end
end

# issue #26083
@testintersect(Base.RefValue{<:Tuple}, Ref{Tuple{M}} where M, Base.RefValue{Tuple{M}} where M)

# issue #31899
struct SA{N,L}
end
@testintersect(Tuple{Type{SA{Int, L} where L}, Type{SA{Int, Int8}}},
               Tuple{Type{<:SA{N, L}}, Type{<:SA{N, L}}} where {N,L},
               Union{})
@testintersect(Tuple{Type{SA{2, L} where L}, Type{SA{2, 16}}},
               Tuple{Type{<:SA{N, L}}, Type{<:SA{N, L}}} where {L,N},
               Union{})
@testintersect(Tuple{Type{SA{2, L} where L}, Type{SA{2, 16}}},
               Tuple{Type{<:SA{N, L}}, Type{<:SA{N, L}}} where {N,L},
               Union{})
@testintersect(Tuple{Type{SA{2, L}}, Type{SA{2, L}}} where L,
               Tuple{Type{<:SA{N, L}}, Type{<:SA{N, L}}} where {N,L},
               Tuple{Type{SA{2, L}}, Type{SA{2, L}}} where L)
@testintersect(Tuple{Type{SA{2, L}}, Type{SA{2, 16}}} where L,
               Tuple{Type{<:SA{N, L}}, Type{<:SA{N, L}}} where {N,L},
               Tuple{Type{SA{2, 16}}, Type{SA{2, 16}}})

# issue #31993
@testintersect(Tuple{Type{<:AbstractVector{T}}, Int} where T,
               Tuple{Type{Vector}, Any},
               Union{})
@testintersect(Tuple{Type{<:AbstractVector{T}}, Int} where T,
               Tuple{Type{Vector{T} where Int<:T<:Int}, Any},
               Tuple{Type{Vector{Int}}, Int})
let X = LinearAlgebra.Symmetric{T, S} where S<:(AbstractArray{U, 2} where U<:T) where T,
    Y = Union{LinearAlgebra.Hermitian{T, S} where S<:(AbstractArray{U, 2} where U<:T) where T,
              LinearAlgebra.Symmetric{T, S} where S<:(AbstractArray{U, 2} where U<:T) where T}
    @test X <: Y
end

# Various nasty varargs
let T1 = Tuple{Int, Tuple{T}, Vararg{T, 3}} where T <: Int,
    T2 = Tuple{Int, Any, Any, Any, Integer},
    T3 = Tuple{Int, Any, Any, Any, Integer, Vararg{Integer}}

    @test issub_strict(T1, T2)
    @test issub_strict(T2, T3)
    @test issub_strict(T1, T3)
end
let A = Tuple{Float64, Vararg{Int64, 2}},
    B1 = Tuple{Float64, Vararg{T, 2}} where T <: Int64,
    B2 = Tuple{Float64, T, T} where T <: Int64,
    C = Tuple{Float64, Any, Vararg{Integer}}

    @test A == B1 == B2
    @test issub_strict(A, C)
    @test issub_strict(B1, C)
    @test issub_strict(B2, C)
end
let B = Tuple{Vararg{Val{N}, N}} where N,
    C = Tuple{Val{2}, Val{2}}

    @test issub(C, B)
end
@test isequal_type(Tuple{T, Vararg{T, 2}} where T<:Real, Tuple{Vararg{T, 3}} where T<: Real)
@test !issub(Tuple{Vararg{T, 3}} where T<:Real, Tuple{Any, Any, Any, Any, Vararg{Any}})
@test !issub(Tuple{Vararg{T, 3}} where T<:Real, Tuple{Any, Any, Any, Any, Vararg{Any, N}} where N)
@test issub_strict(Ref{Tuple{Int, Vararg{Int, N}}} where N, Ref{Tuple{Vararg{Int, N}}} where N)
let T31805 = Tuple{Type{Tuple{}}, Tuple{Vararg{Int8, A}}} where A,
    S31805 = Tuple{Type{Tuple{Vararg{Int32, A}}}, Tuple{Vararg{Int16, A}}} where A
    @test !issub(T31805, S31805)
end

@testintersect(
    Tuple{Array{Tuple{Vararg{Int64,N}},N},Tuple{Vararg{Array{Int64,1},N}}} where N,
    Tuple{Array{Tuple{Int64},1}, Tuple},
    Tuple{Array{Tuple{Int64},1},Tuple{Array{Int64,1}}})

@test !isequal_type(Tuple{Int, Vararg{T, 3}} where T<:Real, Tuple{Int, Real, Vararg{T, 2}} where T<:Integer)

@test !isequal_type(Tuple{Tuple{Vararg{Int}},Tuple{Vararg{Int}}},
                    Tuple{Tuple{Vararg{Int, N}}, Tuple{Vararg{Int, N}}} where N)

let (_, E) = intersection_env(Tuple{Tuple{Vararg{Int}}}, Tuple{Tuple{Vararg{Int,N}}} where N)
    @test !isa(E[1], Type)
end

# this is is a timing test, so it would fail on debug builds
#let T = Type{Tuple{(Union{Int, Nothing} for i = 1:23)..., Union{String, Nothing}}},
#    S = Type{T} where T<:Tuple{E, Vararg{E}} where E
#    @test @elapsed (@test T != S) < 5
#end

# issue #32386
@testintersect(Type{S} where S<:(Vector{Pair{_A,N} where N} where _A),
               Type{Vector{T}} where T,
               Type{Vector{Pair{_A,N} where N}} where _A)

# pr #49049
@testintersect(Tuple{Type{Pair{T, A} where {T, A<:Array{T}}}, Int, Any},
               Tuple{Type{F}, Any, Int} where {F<:(Pair{T, A} where {T, A<:Array{T}})},
               Tuple{Type{Pair{T, A} where {T, A<:(Array{T})}}, Int, Int})

@testintersect(Type{Ref{Union{Int, Tuple{S,S} where S<:T}}} where T,
              Type{F} where F<:(Base.RefValue{Union{Int, Tuple{S,S} where S<:T}} where T),
              Union{})

# issue #32488
struct S32488{S <: Tuple, T, N, L}
    data::NTuple{L,T}
end
@testintersect(Tuple{Type{T} where T<:(S32488{Tuple{_A}, Int64, 1, _A} where _A), Tuple{Vararg{Int64, D}} where D},
               Tuple{Type{S32488{S, T, N, L}}, Tuple{Vararg{T, L}}} where L where N where T where S,
               Tuple{Type{S32488{Tuple{L},Int64,1,L}},Tuple{Vararg{Int64,L}}} where L)

# issue #32703
struct Str{C} <: AbstractString
end
struct CSE{X}
end
const UTF16CSE = CSE{1}
const UTF16Str = Str{UTF16CSE}
const ASCIIStr = Str{CSE{2}}
c32703(::Type{<:Str{UTF16CSE}}, str::AbstractString) = 42
c32703(::Type{<:Str{C}}, str::Str{C}) where {C<:CSE} = str

@testintersect(Tuple{Type{UTF16Str},ASCIIStr},
               Tuple{Type{<:Str{C}}, Str{C}} where {C<:CSE},
               Union{})
@test c32703(UTF16Str, ASCIIStr()) == 42
@testintersect(Tuple{Vector{Vector{Float32}},Matrix,Matrix},
               Tuple{Vector{V},Matrix{Int},Matrix{S}} where {S, V<:AbstractVector{S}},
               Tuple{Array{Array{Float32,1},1},Array{Int,2},Array{Float32,2}})

@testintersect(Tuple{Pair{Int, DataType}, Any},
               Tuple{Pair{A, B} where B<:Type, Int} where A,
               Tuple{Pair{Int, DataType}, Int})

# issue #33337
@test !issub(Tuple{Type{T}, T} where T<:NTuple{30, Union{Nothing, Ref}},
             Tuple{Type{Tuple{Vararg{V}}}, Tuple{Vararg{V}}} where V)
@test  issub(Tuple{Type{Any}, NTuple{4,Union{Int,Nothing}}},
             Tuple{Type{V}, Tuple{Vararg{V}}} where V)

# issue #26065
t26065 = Ref{Tuple{T,Ref{Union{Ref{Tuple{Ref{Union{Ref{Ref{Tuple{Ref{Tuple{Union{Tuple{Ref{Ref{T}},T}, T},T}},T}}}, T}},T}}, Ref{T}, T}}}} where T
s26065 = Ref{Tuple{T,Ref{Union{Ref{Tuple{Ref{Union{Ref{Ref{Tuple{Ref{Tuple{Union{Tuple{Ref{Ref{T}},T}, T},T}},T}}}, T}},T}}, Ref{T}, T}}}} where T
@test t26065 <: s26065

# issue #33894
@testintersect(Tuple{Tuple{Vararg{Any,T}},:N} where T,
               Tuple{Tuple{Vararg{Any,T}},T} where T,
               Union{})
@testintersect(Tuple{Int,Vararg{Any,T}} where T,
               Tuple{T,Vararg{Any,T}} where T,
               Union{})
@testintersect(Tuple{:N,Vararg{Any,T}} where T,
               Tuple{T,Vararg{Any,T}} where T,
               Union{})

@test !issub(Tuple{Type{T}, T} where T<:Tuple{String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}, String, Union{Base.Regex, AbstractChar, AbstractString}},
             Tuple{Type{Tuple{Vararg{V}}}, Tuple{Vararg{V}}} where V)

# issue 36100
@test Pair{(:a, :b), Tuple{Missing, Vararg{Union{},N}} where N} ===
      Pair{(:a, :b), Tuple{Missing, Vararg{Union{},N}} where N} !=
      Pair{(:a, :b), Tuple{Missing, Vararg{Union{}}}} === Pair{(:a, :b), Tuple{Missing}}
@test Val{Tuple{Missing, Vararg{Union{},N}} where N} === Val{Tuple{Missing, Vararg{Union{},N}} where N} !=
      Val{Tuple{Missing, Vararg{Union{}}}} === Val{Tuple{Missing}}

# issue #36869
struct F36869{T, V} <: AbstractArray{Union{T, V}, 1}
end
@testintersect(Tuple{Type{T}, AbstractVector{T}} where T,
               Tuple{Union, F36869{Int64, Missing}},
               Tuple{Union, F36869{Int64, Missing}})

# issue #37180
@test !(typeintersect(Tuple{AbstractArray{T}, VecOrMat{T}} where T, Tuple{Array, Any}).body.parameters[1] isa Union)

# issue #37255
@test Type{Union{}} == Type{T} where {Union{}<:T<:Union{}}

# issue #38081
struct AlmostLU{T, S<:AbstractMatrix{T}}
end
@testintersect(Tuple{AlmostLU, Vector{T}} where T,
               Tuple{AlmostLU{S, X} where X<:Matrix, Vector{S}} where S<:Union{Float32, Float64},
               Tuple{AlmostLU{T, X} where X<:Matrix{T}, Vector{T}} where T<:Union{Float32, Float64})

# issue #22787
@testintersect(Tuple{Type{Q}, Q, Ref{Q}} where Q<:Ref,
               Tuple{Type{S}, Union{Ref{S}, Ref{R}}, R} where R where S,
               Tuple{Type{Q}, Union{Ref{Q}, Ref{R}}, Ref{Q}} where {Q<:Ref, R}) # likely suboptimal

let t = typeintersect(Tuple{Type{T}, T, Ref{T}} where T,
                  Tuple{Type{S}, Ref{S}, S} where S)
    @test_broken t == Tuple{Type{T}, Ref{T}, Ref{T}} where T>:Ref
    @test t == Tuple{Type{T}, Ref{T}, Ref{T}} where T
end

# issue #38279
t = typeintersect(Tuple{<:Array{T, N}, Val{T}} where {T<:Real, N},
                  Tuple{<:Array{T, N}, Val{<:AbstractString}}  where {T<:Real, N})
@test t == Tuple{<:Array{Union{}, N}, Val{Union{}}} where N

# issue #36951
@testintersect(Type{T} where T>:Missing,
               Type{Some{T}} where T,
               Union{})

# issue #38423
let
    Either{L, R} = Union{Ref{L}, Val{R}}
    A = Tuple{Type{Ref{L}}, Type{Either{L, <:Any}}} where L
    B = Tuple{Type{Ref{L2}}, Type{Either{L1, R}}} where {L1, R, L2 <: L1}
    I = typeintersect(A, B)
    @test I != Union{}
    @test_broken I <: A
    @test_broken I <: B
end

# issue #36804
let
    Either{L, R} = Union{Some{L}, Ref{R}}
    f(::Type{Either{L2, R}}, ::Type{Either{L1, R}}) where {L1, R, L2 <: L1} = Either{L1, R}
    f(::Type{Either{L, R1}}, ::Type{Either{L, R2}}) where {L, R1, R2 <: R1} = Either{L, R1}
    @test f(Either{Int,Real}, Either{Int,Float32}) == Either{Int,Real}
end

# issue #36544
let A = Tuple{T, Ref{T}, T} where {T},
    B = Tuple{T, T, Ref{T}} where {T}
    I = typeintersect(A, B)
    @test I != Union{}
    @test_broken I <: A
    @test_broken I <: B
end

# issue #34170
let A = Tuple{Type{T} where T<:Ref, Ref, Union{T, Union{Ref{T}, T}} where T<:Ref},
    B = Tuple{Type{T}, Ref{T}, Union{Int, Ref{T}, T}} where T
    # this was a case where <: disagreed with === (due to a badly-normalized type)
    I = _type_intersect(B, A)
    @test_broken I == Union{Tuple{Type{T}, Ref{T}, Ref{T}} where T<:Ref, Tuple{Type{T}, Ref{T}, T} where T<:Ref}
    @test I == _type_intersect(B, A) == Tuple{Type{T}, Ref{T}, Ref} where T<:Ref
    I = typeintersect(B, A)
    @test_broken I == Tuple{Type{T}, Ref{T}, Union{Ref{T}, T}} where T<:Ref
    @test I == typeintersect(B, A) <: Tuple{Type{T}, Ref{T}, Ref} where T<:Ref

    I = _type_intersect(A, B)
    @test !Base.has_free_typevars(I)
    J = Tuple{Type{T1}, Ref{T1}, Ref} where {T, T1<:Union{Ref, Ref{T}}}
    @test I == _type_intersect(A, B) == J
    @test_broken I == Tuple{Type{T}, Ref{T}, T1} where {T<:Ref, T1<:Union{T, Ref{T}}} # a better result, == to the result with arguments switched

    I = typeintersect(A, B)
    @test !Base.has_free_typevars(I)
    J = Tuple{Type{T1}, Ref{T1}, Ref} where {T, T1<:Union{Ref, Ref{T}}}
    @test I == typeintersect(A, B) == J

end

# issue #39218
let A = Int, B = String, U = Union{A, B}
    @test issub_strict(Union{Tuple{A, A}, Tuple{B, B}}, Tuple{U, U})
    @test issub_strict(Union{Tuple{A, A}, Tuple{B, B}}, Tuple{Union{A, B}, Union{A, B}})
end

struct A39218 end
struct B39218 end
const AB39218 = Union{A39218,B39218}
f39218(::T, ::T) where {T<:AB39218} = false
g39218(a, b) = (@nospecialize; if a isa AB39218 && b isa AB39218; f39218(a, b); end;)
@test g39218(A39218(), A39218()) === false
@test_throws MethodError g39218(A39218(), B39218())

# issue #39521
@test Tuple{Type{Tuple{A}} where A, DataType, DataType} <: Tuple{Vararg{B}} where B
@test Tuple{DataType, Type{Tuple{A}} where A, DataType} <: Tuple{Vararg{B}} where B

let A = Tuple{Type{<:Union{Number, T}}, Ref{T}} where T,
    B = Tuple{Type{<:Union{Number, T}}, Ref{T}} where T
    # TODO: these are caught by the egal check, but the core algorithm gets them wrong
    @test A == B
    @test A <: B
end

# issue #39698
@testintersect(Type{T} where T<:(AbstractArray{I}) where I<:(Base.IteratorsMD.CartesianIndex),
    Type{S} where S<:(Base.IteratorsMD.CartesianIndices{A, B} where B<:Tuple{Vararg{Any, A}} where A),
    Type{S} where {N, S<:(Base.IteratorsMD.CartesianIndices{N, B} where B<:Tuple{Vararg{Any, N}})})

# issue #39948
@testintersect(Tuple{Array{Pair{T, JT} where JT<:Ref{T}, 1} where T, Vector},
    Tuple{Vararg{Vector{T}}} where T,
    Tuple{Array{Pair{T, JT} where JT<:Ref{T}, 1}, Array{Pair{T, JT} where JT<:Ref{T}, 1}} where T)

# issue #8915
struct D8915{T<:Union{Float32,Float64}}
    D8915{T}(a) where {T} = 1
    D8915{T}(a::Int) where {T} = 2
end
@test D8915{Float64}(1) == 2
@test D8915{Float64}(1.0) == 1

# issue #18985
f18985(x::T, y...) where {T<:Union{Int32,Int64}} = (length(y), f18985(y[1], y[2:end]...)...)
f18985(x::T) where {T<:Union{Int32,Int64}} = 100
@test f18985(1, 2, 3) == (2, 1, 100)

# issue #40048
let A = Tuple{Ref{T}, Vararg{T}} where T,
    B = Tuple{Ref{U}, Union{Ref{S}, Ref{U}, Int}, Union{Ref{S}, S}} where S where U,
    C = Tuple{Ref{U}, Union{Ref{S}, Ref{U}, Ref{W}}, Union{Ref{S}, W, V}} where V<:AbstractArray where W where S where U
    I = typeintersect(A, B)
    Ts = (Tuple{Ref{Int}, Int, Int}, Tuple{Ref{Ref{Int}}, Ref{Int}, Ref{Int}})
    @test I != Union{}
    @test_broken I <: A
    @test I <: B
    for T in Ts
        if T <: A && T <: B
            @test T <: I
        end
    end
    J = typeintersect(A, C)
    @test J != Union{}
    @test_broken J <: A
    @test J <: C
    for T in Ts
        if T <: A && T <: C
            @test T <: J
        end
    end
end

let A = Tuple{Dict{I,T}, I, T} where T where I,
    B = Tuple{AbstractDict{I,T}, T, I} where T where I,
    I = typeintersect(A, B)
    # TODO: we should probably have something approaching I == T here,
    # though note something more complex is needed since the intersection must also include types such as;
    # Tuple{Dict{Integer,Any}, Integer, Int}
    @test_broken I <: A && I <: B
    @test I == typeintersect(B, A) == Tuple{Dict{I, T}, Any, Any} where {I, T}
end

let A = Tuple{UnionAll, Vector{Any}},
    B = Tuple{Type{T}, T} where T<:AbstractArray,
    I = typeintersect(A, B)
    @test !isconcretetype(I)
    @test I == Tuple{Type{T}, Vector{Any}} where T<:AbstractArray
end

@testintersect(Tuple{Type{Vector{<:T}}, T} where {T<:Integer},
               Tuple{Type{T}, AbstractArray} where T<:Array,
               Bottom)

struct S40{_A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P, _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z, _Z1, _Z2, _Z3, _Z4, _Z5, _Z6, _Z7, _Z8, _Z9, _Z10, _Z11, _Z12, _Z13, _Z14}
end

@testintersect(Tuple{Type{S40{_A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P, _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z, _Z1, _Z2, _Z3, _Z4, _Z5, _Z6, _Z7, _Z8, _Z9, _Z10, _Z11, _Z12, _Z13, _Z14}} where _Z14 where _Z13 where _Z12 where _Z11 where _Z10 where _Z9 where _Z8 where _Z7 where _Z6 where _Z5 where _Z4 where _Z3 where _Z2 where _Z1 where _Z where _Y where _X where _W where _V where _U where _T where _S where _R where _Q where _P where _O where _N where _M where _L where _K where _J where _I where _H where _G where _F where _E where _D where _C where _B where _A, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any},
               Tuple{Type{S40{A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40} where A40 where A39 where A38 where A37 where A36 where A35 where A34 where A33 where A32 where A31 where A30 where A29 where A28 where A27 where A26 where A25 where A24 where A23 where A22 where A21 where A20 where A19 where A18 where A17 where A16 where A15 where A14 where A13 where A12 where A11 where A10 where A9 where A8 where A7 where A6 where A5 where A4 where A3 where A2 where A1}, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, A31, A32, A33, A34, A35, A36, A37, A38, A39, A40} where A40 where A39 where A38 where A37 where A36 where A35 where A34 where A33 where A32 where A31 where A30 where A29 where A28 where A27 where A26 where A25 where A24 where A23 where A22 where A21 where A20 where A19 where A18 where A17 where A16 where A15 where A14 where A13 where A12 where A11 where A10 where A9 where A8 where A7 where A6 where A5 where A4 where A3 where A2 where A1,
               Bottom)

let A = Tuple{Any, Type{Ref{_A}} where _A},
    B = Tuple{Type{T}, Type{<:Union{Ref{T}, T}}} where T,
    I = typeintersect(A, B)
    @test I != Union{}
    @test Tuple{Type{Ref{Integer}}, Type{Ref{Integer}}} <: I
    # TODO: this intersection result seems too wide (I == B) ?
    @test_broken !<:(Tuple{Type{Int}, Type{Int}}, I)
end

@testintersect(Tuple{Type{T}, T} where T<:(Tuple{Vararg{_A, _B}} where _B where _A),
               Tuple{Type{Tuple{Vararg{_A, N}} where _A<:F}, Pair{N, F}} where F where N,
               Bottom)

# issue #42409
@testintersect(Tuple{Type{Pair{_A, S} where S<:AbstractArray{<:_A, 2}}, Dict} where _A,
               Tuple{Type{Pair{_A, S} where S<:AbstractArray{<:_A, 2}} where _A, Union{Array, Pair}},
               Bottom)

# https://github.com/JuliaLang/julia/issues/44735
@test_throws TypeError(:typeassert, Type, Vararg{Int}) typeintersect(Vararg{Int}, Int)
@test_throws TypeError(:typeassert, Type, Vararg{Int}) typeintersect(Int, Vararg{Int})
@test_throws TypeError(:typeassert, Type, 1) typeintersect(1, Int)
@test_throws TypeError(:typeassert, Type, 1) typeintersect(Int, 1)

let A = Tuple{typeof(identity), Type{Union{}}},
    B = Tuple{typeof(identity), typeof(Union{})}
    @test A == B && (Base.isdispatchtuple(A) == Base.isdispatchtuple(B))
end

# issue #45703
# requires assertions enabled (to catch discrepancy in obvious_subtype)
let T = TypeVar(:T, Real),
    V = TypeVar(:V, AbstractVector{T}),
    S = Type{Pair{T, V}}
    @test !(UnionAll(T, UnionAll(V, UnionAll(T, Type{Pair{T, V}}))) <: UnionAll(T, UnionAll(V, Type{Pair{T, V}})))
    @test !(UnionAll(T, UnionAll(V, UnionAll(T, S))) <: UnionAll(T, UnionAll(V, S)))
end

# issue #41096
let C = Val{Val{B}} where {B}
    @testintersect(Val{<:Union{Missing, Val{false}, Val{true}}}, C, Val{<:Union{Val{true}, Val{false}}})
    @testintersect(Val{<:Union{Nothing, Val{true}, Val{false}}}, C, Val{<:Union{Val{true}, Val{false}}})
    @testintersect(Val{<:Union{Nothing, Val{false}}}, C, Val{Val{false}})
end

#issue #43082
struct X43082{A, I, B<:Union{Ref{I},I}}; end
@testintersect(Tuple{X43082{T}, Int} where T, Tuple{X43082{Int}, Any}, Tuple{X43082{Int}, Int})

#issue #36443
let C = Tuple{Val{3},Int,Int,Int},
    As = (Tuple{Val{N},Vararg{T,N}} where {T,N},
          Tuple{Val{N},Vararg{T,N}} where {N,T}),
    Bs = (Tuple{Val{3},Int,Vararg{T,N}} where {T,N},
          Tuple{Val{3},Int,Vararg{T,N}} where {N,T},
          Tuple{Val{3},Int,Vararg{T}} where {T},
          Tuple{Val{3},Int,Vararg{T,2}} where {T})
    for A in As, B in Bs
        @testintersect(A, B, C)
    end
end

let A = Tuple{Type{Val{N}},Tuple{Vararg{T,N}} where T} where N,
    C = Tuple{Type{Val{2}},Tuple{T,T} where T}
    @testintersect(A, Tuple{Type{Val{2}},Tuple{Vararg{T,N}} where T} where N, C)
    @testintersect(A, Tuple{Type{Val{2}},Tuple{T,Vararg{T,N}} where T} where N, C)
    @testintersect(A, Tuple{Type{Val{2}},Tuple{T,T,Vararg{T,N}} where T} where N, C)
end

let f36443(::NTuple{N}=[(f36443,),(1,2)][2],::Val{N}=Val(2)) where{N} = 0
    @test f36443() == 0;
end

let C = Tuple{Val{3},Int,Int,Int,Int},
    As = (Tuple{Val{N},Int,Vararg{T,N}} where {T,N},
          Tuple{Val{N},Int,Vararg{T,N}} where {N,T}),
    Bs = (Tuple{Val{3},Vararg{T,N}} where {T,N},
          Tuple{Val{3},Vararg{T,N}} where {N,T},
          Tuple{Val{3},Vararg{T}} where {T})
    for A in As, B in Bs
        @testintersect(A, B, C)
    end
end

#issue #37257
let T = Tuple{Val{N}, Any, Any, Vararg{Any,N}} where N,
    C = Tuple{Val{1}, Any, Any, Any}
    @testintersect(T, Tuple{Val{1}, Vararg{Any}}, C)
    @testintersect(T, Tuple{Val{1}, Any, Vararg{Any}}, C)
    @testintersect(T, Tuple{Val{1}, Any, Any, Vararg{Any}}, C)
    @testintersect(T, Tuple{Val{1}, Any, Any, Any, Vararg{Any}}, C)
    @testintersect(T, Tuple{Val{1}, Any, Any, Any, Any, Vararg{Any}}, Union{})
end

let A = Tuple{NTuple{N,Any},Val{N}} where {N},
    C = Tuple{NTuple{4,Any},Val{4}}
    @testintersect(A, Tuple{Tuple{Vararg{Any,N}},Val{4}} where {N}, C)
    @testintersect(A, Tuple{Tuple{Vararg{Any}},Val{4}}, C)
    @testintersect(A, Tuple{Tuple{Vararg{Any,N}} where {N},Val{4}}, C)

    @testintersect(A, Tuple{Tuple{Any,Vararg{Any,N}},Val{4}} where {N}, C)
    @testintersect(A, Tuple{Tuple{Any,Vararg{Any}},Val{4}}, C)
    @testintersect(A, Tuple{Tuple{Any,Vararg{Any,N}} where {N},Val{4}}, C)

    @testintersect(A, Tuple{Tuple{Any,Any,Any,Any,Any,Vararg{Any,N}},Val{4}} where {N}, Union{})
    @testintersect(A, Tuple{Tuple{Any,Any,Any,Any,Any,Vararg{Any}},Val{4}}, Union{})
    @testintersect(A, Tuple{Tuple{Any,Any,Any,Any,Any,Vararg{Any,N}} where {N},Val{4}}, Union{})
end

#issue #39088
let
    a() = c((1,), (1,1,1,1))
    c(d::NTuple{T}, ::NTuple{T}) where T = d
    c(d::NTuple{f}, b) where f = c((d..., f), b)
    j(h::NTuple{T}, ::NTuple{T} = a()) where T = nothing
    @test j((1,1,1,1)) === nothing
end

let A = Tuple{NTuple{N, Int}, NTuple{N, Int}} where N,
    C = Tuple{NTuple{4, Int}, NTuple{4, Int}}
    @testintersect(A, Tuple{Tuple{Int, Vararg{Any}}, NTuple{4, Int}}, C)
    @testintersect(A, Tuple{Tuple{Int, Vararg{Any, N}} where {N}, NTuple{4, Int}}, C)
    @testintersect(A, Tuple{Tuple{Int, Vararg{Any, N}}, NTuple{4, Int}} where {N}, C)

    Bs = (Tuple{Tuple{Int, Vararg{Any}}, Tuple{Int, Int, Vararg{Any}}},
          Tuple{Tuple{Int, Vararg{Any,N1}}, Tuple{Int, Int, Vararg{Any,N2}}} where {N1,N2},
          Tuple{Tuple{Int, Vararg{Any,N}} where {N}, Tuple{Int, Int, Vararg{Any,N}} where {N}})
    Cerr = Tuple{Tuple{Int, Vararg{Int, N}}, Tuple{Int, Int, Vararg{Int, N}}} where {N}
    for B in Bs
        C = typeintersect(A, B)
        @test C == typeintersect(B, A) != Union{}
        @test C != Cerr
        # TODO: The ideal result is Tuple{Tuple{Int, Int, Vararg{Int, N}}, Tuple{Int, Int, Vararg{Int, N}}} where {N}
        @test_broken C != Tuple{Tuple{Int, Vararg{Int}}, Tuple{Int, Int, Vararg{Int}}}
    end
end

let A = Pair{NTuple{N, Int}, NTuple{N, Int}} where N,
    C = Pair{NTuple{4, Int}, NTuple{4, Int}}
    @testintersect(A, Pair{<:Tuple{Int, Vararg{Any}}, NTuple{4, Int}}, C)
    @testintersect(A, Pair{<:Tuple{Int, Vararg{Any, N}} where {N}, NTuple{4, Int}}, C)
    @testintersect(A, Pair{<:Tuple{Int, Vararg{Any, N}}, NTuple{4, Int}} where {N}, C)

    Bs = (Pair{<:Tuple{Int, Vararg{Int}}, <:Tuple{Int, Int, Vararg{Int}}},
          Pair{Tuple{Int, Vararg{Int,N1}}, Tuple{Int, Int, Vararg{Int,N2}}} where {N1,N2},
          Pair{<:Tuple{Int, Vararg{Int,N}} where {N}, <:Tuple{Int, Int, Vararg{Int,N}} where {N}})
    Cs = (Bs[2], Bs[2], Bs[3])
    for (B, C) in zip(Bs, Cs)
        # TODO: The ideal result is Pair{Tuple{Int, Int, Vararg{Int, N}}, Tuple{Int, Int, Vararg{Int, N}}} where {N}
        @testintersect(A, B, C)
    end
end

# Example from pr#39098
@testintersect(NTuple, Tuple{Any,Vararg}, Tuple{T, Vararg{T}} where {T})

@testintersect(Val{T} where T<:Tuple{Tuple{Any, Vararg{Any}}},
               Val{Tuple{Tuple{Vararg{Any, N}}}} where {N},
               Val{Tuple{Tuple{Any, Vararg{Any, N}}}} where {N})

let A = Pair{NTuple{N, Int}, Val{N}} where N,
    C = Pair{Tuple{Int, Vararg{Int,N1}}, Val{N2}} where {N1,N2},
    B = Pair{<:Tuple{Int, Vararg{Int}}, <:Val}
    @testintersect A B C
    @testintersect A C C
end

# issue #49484
let S = Tuple{Integer, U} where {II<:Array, U<:Tuple{Vararg{II, 1}}}
    T = Tuple{Int, U} where {II<:Array, U<:Tuple{Vararg{II, 1}}}
    @testintersect(S, Tuple{Int, U} where {U<:Tuple{Vararg{Any}}}, T)
    @testintersect(S, Tuple{Int, U} where {N, U<:Tuple{Vararg{Any,N}}}, T)
    @testintersect(S, Tuple{Int, U} where {U<:Tuple{Any,Vararg{Any}}}, T)
    @testintersect(S, Tuple{Int, U} where {N, U<:Tuple{Any,Vararg{Any,N}}}, T)
    @testintersect(S, Tuple{Int, U} where {U<:Tuple{Any,Any,Vararg{Any}}}, Union{})
    @testintersect(S, Tuple{Int, U} where {N, U<:Tuple{Any,Any,Vararg{Any,N}}}, Union{})
end

# issue #43064
let
    env_tuple(@nospecialize(x), @nospecialize(y)) = (intersection_env(x, y)[2]...,)
    all_var(x::UnionAll) = (x.var, all_var(x.body)...)
    all_var(x::DataType) = ()
    TT0 = Tuple{Type{T},Union{Real,Missing,Nothing}} where {T}
    TT1 = Union{Type{Int8},Type{Int16}}
    @test env_tuple(Tuple{TT1,Missing}, TT0) ===
          env_tuple(Tuple{TT1,Nothing}, TT0) ===
          env_tuple(Tuple{TT1,Int}, TT0) === all_var(TT0)

    TT0 = Tuple{T1,T2,Union{Real,Missing,Nothing}} where {T1,T2}
    TT1 = Tuple{T1,T2,Union{Real,Missing,Nothing}} where {T2,T1}
    TT2 = Tuple{Union{Int,Int8},Union{Int,Int8},Int}
    TT3 = Tuple{Int,Union{Int,Int8},Int}
    @test env_tuple(TT2, TT0) === all_var(TT0)
    @test env_tuple(TT2, TT1) === all_var(TT1)
    @test env_tuple(TT3, TT0) === Base.setindex(all_var(TT0), Int, 1)
    @test env_tuple(TT3, TT1) === Base.setindex(all_var(TT1), Int, 2)

    TT0 = Tuple{T1,T2,T1,Union{Real,Missing,Nothing}} where {T1,T2}
    TT1 = Tuple{T1,T2,T1,Union{Real,Missing,Nothing}} where {T2,T1}
    TT2 = Tuple{Int,Union{Int,Int8},Int,Int}
    @test env_tuple(TT2, TT0) === Base.setindex(all_var(TT0), Int, 1)
    @test env_tuple(TT2, TT1) === Base.setindex(all_var(TT1), Int, 2)
end

#issue #46735
T46735{B<:Real} = Pair{<:Union{B, Val{<:B}}, <:Union{AbstractMatrix{B}, AbstractMatrix{Vector{B}}}}
@testintersect(T46735{B} where {B}, T46735, !Union{})
@testintersect(T46735{B} where {B<:Integer}, T46735, !Union{})
S46735{B<:Val, M<:AbstractMatrix} = Tuple{<:Union{B, <:Val{<:B}},M,<:(Union{AbstractMatrix{B}, AbstractMatrix{<:Vector{<:B}}})}
@testintersect(S46735{B} where {B}, S46735, !Union{})
@testintersect(S46735{B, M} where {B, M}, S46735, !Union{})
A46735{B<:Val, M<:AbstractMatrix} = Tuple{<:Union{B, <:Val{<:B}},M,Union{AbstractMatrix{B}, AbstractMatrix{<:Vector{<:B}}}}
@testintersect(A46735{B} where {B}, A46735, !Union{})
@testintersect(A46735{B, M} where {B, M}, A46735, !Union{})

#issue #46871 #38497
struct A46871{T, N, M} <: AbstractArray{T, N} end
struct B46871{T, N} <: Ref{A46871{T, N, N}} end
for T in (B46871{Int, N} where {N}, B46871{Int}) # intentional duplication
    @testintersect(T, Ref{<:AbstractArray{<:Real, 3}}, B46871{Int, 3})
end
abstract type C38497{e,g<:Tuple,i} end
struct Q38497{o,e<:NTuple{o},g} <: C38497{e,g,Array{o}} end
@testintersect(Q38497{<:Any, Tuple{Int}}, C38497, Q38497{<:Any, Tuple{Int}, <:Tuple})
# n.b. the only concrete instance of this type is Q38497{1, Tuple{Int}, <:Tuple} (since NTuple{o} also adds an ::Int constraint)
# but this abstract type is also part of the intersection abstractly

abstract type X38497{T<:Number} end
abstract type Y38497{T>:Integer} <: X38497{T} end
struct Z38497{T>:Int} <: Y38497{T} end
@testintersect(Z38497, X38497, Z38497{T} where Int<:T<:Number)
@testintersect(Z38497, Y38497, Z38497{T} where T>:Integer)
@testintersect(X38497, Y38497, Y38497{T} where Integer<:T<:Number)

#issue #33138
@test Vector{Vector{Tuple{T,T}} where Int<:T<:Int} <: Vector{Vector{Tuple{S1,S1} where S<:S1<:S}} where S

#issue #46970
@test only(intersection_env(Union{S, Matrix{Int}} where S<:Matrix, Matrix)[2]) isa TypeVar
T46784{B<:Val, M<:AbstractMatrix} = Tuple{<:Union{B, <:Val{<:B}}, M, Union{AbstractMatrix{B}, AbstractMatrix{<:Vector{<:B}}}}
@testintersect(T46784{T,S} where {T,S}, T46784, !Union{})
@test T46784 <: T46784{T,S} where {T,S}

#issue 36185
let S = Tuple{Type{T},Array{Union{T,Missing},N}} where {T,N},
    T = Tuple{Type{T},Array{Union{T,Nothing},N}} where {T,N}
    I = typeintersect(S, T)
    @test I == typeintersect(T, S) != Union{}
    @test_broken I <: S
    @test_broken I <: T
end

#issue 46736
let S = Tuple{Val{T}, T} where {S1,T<:Val{Union{Nothing,S1}}},
    T = Tuple{Val{Val{Union{Nothing, S2}}}, Any} where S2
    @testintersect(S, T, !Union{})
    # not ideal (`S1` should be unbounded)
    @test_broken testintersect(S, T) == Tuple{Val{Val{Union{Nothing, S1}}}, Val{Union{Nothing, S1}}} where S1<:(Union{Nothing, S2} where S2)
end

#issue #47874:case1
let S1 = Tuple{Int, Any, Union{Val{C1}, C1}} where {R1<:Real, C1<:Union{Complex{R1}, R1}},
    S2 = Tuple{Int, Any, Union{Val{C1}, C1} where {R1<:Real, C1<:Union{Complex{R1}, R1}}},
    T1 = Tuple{Any, Int, Union{Val{C2}, C2}} where {R2<:Real, C2<:Union{Complex{R2}, R2}},
    T2 = Tuple{Any, Int, V} where {R2<:Real, C2<:Union{Complex{R2}, R2}, V<:Union{Val{C2}, C2}}
    for S in (S1, S2), T in (T1, T2)
        @testintersect(S, T, !Union{})
    end
end

#issue #47874:case2
let S = Tuple{Int, Vararg{Val{C} where C<:Union{Complex{R}, R}}} where R
    T = Tuple{Any, Vararg{Val{C} where C<:Union{Complex{R}, R}}} where R<:Real
    I  = Tuple{Any, Vararg{Val{C} where C<:Union{Complex{R}, R}}} where R<:Real
    @testintersect(S, T, !Union{})
    @test_broken typeintersect(S, T) == I
    @test_broken typeintersect(T, S) == I
end

#issue #47874:case3
let S = Tuple{Int, Tuple{Vararg{Val{C1} where C1<:Union{Complex{R1}, R1}}} where R1<:(Union{Real, V1} where V1), Tuple{Vararg{Val{C2} where C2<:Union{Complex{R2}, Complex{R3}, R3}}} where {R2<:(Union{Real, V2} where V2), R3<:Union{Complex{R2}, Real, R2}}},
    T = Tuple{Any, Tuple{Vararg{Val{CC1} where CC1<:Union{Complex{R}, R}}}, Tuple{Vararg{Val{CC2} where CC2<:Union{Complex{R}, R}}}} where R<:Real
    @testintersect(S, T, !Union{})
end

let S = Tuple{T2, V2} where {T2, N2, V2<:(Array{S2, N2} where {S2 <: T2})},
    T = Tuple{V1, T1} where {T1, N1, V1<:(Array{S1, N1} where {S1 <: T1})}
    @testintersect(S, T, !Union{})
end

let S = Dict{Int, S1} where {F1, S1<:Union{Int8, Val{F1}}},
    T = Dict{F2, S2} where {F2, S2<:Union{Int8, Val{F2}}}
    @test_broken typeintersect(S, T) == Dict{Int, S} where S<:Union{Val{Int}, Int8}
    @test typeintersect(T, S) == Dict{Int, S} where S<:Union{Val{Int}, Int8}
end

# Ensure inner `intersect_all` never under-esitimate.
let S = Tuple{F1, Dict{Int, S1}} where {F1, S1<:Union{Int8, Val{F1}}},
    T = Tuple{Any, Dict{F2, S2}} where {F2, S2<:Union{Int8, Val{F2}}}
    @test Tuple{Nothing, Dict{Int, Int8}} <: S
    @test Tuple{Nothing, Dict{Int, Int8}} <: T
    @test Tuple{Nothing, Dict{Int, Int8}} <: typeintersect(S, T)
    @test Tuple{Nothing, Dict{Int, Int8}} <: typeintersect(T, S)
end

let S = Tuple{F1, Val{S1}} where {F1, S1<:Dict{F1}}
    T = Tuple{Any, Val{S2}} where {F2, S2<:Union{map(T->Dict{T}, Base.BitInteger_types)...}}
    ST = typeintersect(S, T)
    TS = typeintersect(S, T)
    for U in Base.BitInteger_types
        @test Tuple{U, Val{Dict{U,Nothing}}} <: S
        @test Tuple{U, Val{Dict{U,Nothing}}} <: T
        @test Tuple{U, Val{Dict{U,Nothing}}} <: ST
        @test Tuple{U, Val{Dict{U,Nothing}}} <: TS
    end
end

#issue 55206
struct T55206{A,B<:Complex{A},C<:Union{Dict{Nothing},Dict{A}}} end
@testintersect(T55206, T55206{<:Any,<:Any,<:Dict{Nothing}}, T55206{A,<:Complex{A},<:Dict{Nothing}} where {A})
@testintersect(
    Tuple{Dict{Int8, Int16}, Val{S1}} where {F1, S1<:AbstractSet{F1}},
    Tuple{Dict{T1, T2}, Val{S2}} where {T1, T2, S2<:Union{Set{T1},Set{T2}}},
    Tuple{Dict{Int8, Int16}, Val{S1}} where {S1<:Union{Set{Int8},Set{Int16}}}
)

f48167(::Type{Val{L2}}, ::Type{Union{Val{L1}, Set{R}}}) where {L1, R, L2<:L1} = 1
f48167(::Type{Val{L1}}, ::Type{Union{Val{L2}, Set{R}}}) where {L1, R, L2<:L1} = 2
f48167(::Type{Val{L}}, ::Type{Union{Val{L}, Set{R}}}) where {L, R} = 3
@test f48167(Val{Nothing}, Union{Val{Nothing}, Set{Int}}) == 3

# https://github.com/JuliaLang/julia/pull/31167#issuecomment-1358381818
let S = Tuple{Type{T1}, T1, Val{T1}} where T1<:(Val{S1} where S1<:Val),
    T = Tuple{Union{Type{T2}, Type{S2}}, Union{Val{T2}, Val{S2}}, Union{Val{T2}, S2}} where T2<:Val{A2} where A2 where S2<:Val
    I1 = typeintersect(S, T)
    I2 = typeintersect(T, S)
    @test I1 !== Union{} && I2 !== Union{}
    @test_broken I1 <: S
    @test_broken I2 <: T
    @test_broken I2 <: S
    @test_broken I2 <: T
end

#issue 44395
@testintersect(Tuple{Type{T}, T} where {T <: Vector{Union{T, R}} where {R<:Real, T<:Real}},
               Tuple{Type{Vector{Union{T, R}}}, Matrix{Union{T, R}}} where {R<:Real, T<:Real},
               Union{})

#issue 26487
@testintersect(Tuple{Type{Tuple{T,Val{T}}}, Val{T}} where T,
               Tuple{Type{Tuple{Val{T},T}}, Val{T}} where T,
               Union{})

@test only(intersection_env(Val{Union{Val{Val{T}} where {T},Int}}, Val{Union{T,Int}} where T)[2]) === Val{Val{T}} where {T}

# issue 47654
Vec47654{T} = Union{AbstractVector{T}, AbstractVector{Union{T,Nothing}}}
struct Wrapper47654{T, V<:Vec47654{T}}
    v::V
end
abstract type P47654{A} end
@test Wrapper47654{P47654, Vector{Union{P47654,Nothing}}} <: Wrapper47654

#issue 41561
@testintersect(Tuple{Vector{VT}, Vector{VT}} where {N1, VT<:AbstractVector{N1}},
               Tuple{Vector{VN} where {N, VN<:AbstractVector{N}}, Vector{Vector{Float64}}},
               Tuple{Vector{Vector{Float64}}, Vector{Vector{Float64}}})

@testset "known subtype/intersect issue" begin
    #issue 45874
    let S = Pair{Val{P}, AbstractVector{<:Union{P,<:AbstractMatrix{P}}}} where P,
        T = Pair{Val{R}, AbstractVector{<:Union{P,<:AbstractMatrix{P}}}} where {P,R}
        @test S <: T
    end

    #issue 40865
    @test Tuple{Set{Ref{Int}}, Set{Ref{Int}}} <: Tuple{Set{KV}, Set{K}} where {K,KV<:Union{K,Ref{K}}}
    @test Tuple{Set{Val{Int}}, Set{Val{Int}}} <: Tuple{Set{KV}, Set{K}} where {K,KV<:Union{K,Val{K}}}

    #issue 39099
    A = Tuple{Tuple{Int, Int, Vararg{Int, N}}, Tuple{Int, Vararg{Int, N}}, Tuple{Vararg{Int, N}}} where N
    B = Tuple{NTuple{N, Int}, NTuple{N, Int}, NTuple{N, Int}} where N
    @test_broken !(A <: B)

    #issue 35698
    @test_broken typeintersect(Type{Tuple{Array{T,1} where T}}, UnionAll) != Union{}

    #issue 33137
    @test_broken (Tuple{Q,Int} where Q<:Int) <: Tuple{T,T} where T

    # issue 24333
    @test (Type{Union{Ref,Cvoid}} <: Type{Union{T,Cvoid}} where T)

    # issue 22123
    t1 = Ref{Ref{Ref{Union{Int64, T}}} where T}
    t2 = Ref{Ref{Ref{Union{T, S}}} where T} where S
    @test t1 <: t2

    # issue 21153
    @test_broken (Tuple{T1,T1} where T1<:(Val{T2} where T2)) <: (Tuple{Val{S},Val{S}} where S)
end

# issue #47658
let T = Ref{NTuple{8, Ref{Union{Int, P}}}} where P,
    S = Ref{NTuple{8, Ref{Union{Int, P}}}} where P
    # note T and S are identical but we need 2 copies to avoid being fooled by pointer equality
    @test T <: Union{Int, S}
end

# try to fool a greedy algorithm that picks X=Int, Y=String here
@test Tuple{Ref{Union{Int,String}}, Ref{Union{Int,String}}} <: Tuple{Ref{Union{X,Y}}, Ref{X}} where {X,Y}
@test Tuple{Ref{Union{Int,String,Missing}}, Ref{Union{Int,String}}} <: Tuple{Ref{Union{X,Y}}, Ref{X}} where {X,Y}

@test !(Tuple{Any, Any, Any} <: Tuple{Any, Vararg{T}} where T)

# issue #39967
@test (NTuple{27, T} where {S, T<:Union{Array, Array{S}}}) <: Tuple{Array, Array, Vararg{AbstractArray, 25}}

abstract type MyAbstract47877{C}; end
struct MyType47877{A,B} <: MyAbstract47877{A} end
let A = Tuple{Type{T}, T} where T,
    B = Tuple{Type{MyType47877{W, V} where V<:Union{Base.BitInteger, MyAbstract47877{W}}}, MyAbstract47877{<:Base.BitInteger}} where W
    C = Tuple{Type{MyType47877{W, V} where V<:Union{MyAbstract47877{W}, Base.BitInteger}}, MyType47877{W, V} where V<:Union{MyAbstract47877{W}, Base.BitInteger}} where W<:Base.BitInteger
    # ensure that merge_env for innervars does not blow up (the large Unions ensure this will take excessive memory if it does)
    @testintersect(A, B, C)
end

let
    a = (isodd(i) ? Pair{Char, String} : Pair{String, String} for i in 1:2000)
    @test Tuple{Type{Pair{Union{Char, String}, String}}, a...} <: Tuple{Type{Pair{K, V}}, Vararg{Pair{A, B} where B where A}} where V where K
    a = (isodd(i) ? Matrix{Int} : Vector{Int} for i in 1:4000)
    @test Tuple{Type{Pair{Union{Char, String}, String}}, a...,} <: Tuple{Type{Pair{K, V}}, Vararg{Array}} where V where K
end

#issue 48582
@test !<:(Tuple{Pair{<:T,<:T}, Val{S} where {S}} where {T<:Base.BitInteger},
          Tuple{Pair{<:T,<:T}, Val{Int}} where {T<:Base.BitInteger})

struct T48695{T, N, H<:AbstractArray} <: AbstractArray{Union{Missing, T}, N} end
struct S48695{T, N, H<:AbstractArray{T, N}} <: AbstractArray{T, N} end
let S = Tuple{Type{S48695{T, 2, T48695{B, 2, C}}} where {T<:(Union{Missing, A} where A), B, C}, T48695{T, 2} where T},
    T = Tuple{Type{S48695{T, N, H}}, H} where {T, N, H<:AbstractArray{T, N}}
    V = typeintersect(S, T)
    vars_in_unionall(s) = s isa UnionAll ? (s.var, vars_in_unionall(s.body)...) : ()
    @test V != Union{}
    @test allunique(vars_in_unionall(V))
    @test typeintersect(V, T) != Union{}
end

#issue 48961
@test !<:(Type{Union{Missing, Int}}, Type{Union{Missing, Nothing, Int}})

#issue 49127
struct F49127{m,n} <: Function end
let a = [TypeVar(:V, Union{}, Function) for i in 1:32]
    b = a[1:end-1]
    S = foldr((v, d) -> UnionAll(v, d), a; init = foldl((i, j) -> F49127{i, j}, a))
    T = foldr((v, d) -> UnionAll(v, d), b; init = foldl((i, j) -> F49127{i, j}, b))
    @test S <: T
end

# requires assertions enabled (to test union-split in `obviously_disjoint`)
@test !<:(Tuple{Type{Int}, Int}, Tuple{Type{Union{Int, T}}, T} where T<:Union{Int8,Int16})
@test <:(Tuple{Type{Int}, Int}, Tuple{Type{Union{Int, T}}, T} where T<:Union{Int8,Int})

#issue #49354 (requires assertions enabled)
@test !<:(Tuple{Type{Union{Int, Val{1}}}, Int}, Tuple{Type{Union{Int, T1}}, T1} where T1<:Val)
@test !<:(Tuple{Type{Union{Int, Val{1}}}, Int}, Tuple{Type{Union{Int, T1}}, T1} where T1<:Union{Val,Pair})
@test <:(Tuple{Type{Union{Int, Val{1}}}, Int}, Tuple{Type{Union{Int, T1}}, T1} where T1<:Union{Integer,Val})
@test <:(Tuple{Type{Union{Int, Int8}}, Int}, Tuple{Type{Union{Int, T1}}, T1} where T1<:Integer)
@test !<:(Tuple{Type{Union{Pair{Int, Any}, Pair{Int, Int}}}, Pair{Int, Any}},
          Tuple{Type{Union{Pair{Int, Any}, T1}}, T1} where T1<:(Pair{T,T} where {T}))

let A = Tuple{Type{T}, T, Val{T}} where T,
    B = Tuple{Type{S}, Val{S}, Val{S}} where S
    @test_broken typeintersect(A, B) == Tuple{Type{T}, Val{T}, Val{T}} where T>:Val
    @test typeintersect(A, B) <: Tuple{Type{T}, Val{T}, Val{T}} where T
end
let A = Tuple{Type{T}, T, Val{T}} where T<:Val,
    B = Tuple{Type{S}, Val{S}, Val{S}} where S<:Val
    @test_broken typeintersect(A, B) == Tuple{Type{Val}, Val{Val}, Val{Val}}
    @test typeintersect(A, B) <: Tuple{Type{T}, Val{T}, Val{T}} where T<:Val
end
let A = Tuple{Type{T}, T, Val{T}} where T<:Val,
    B = Tuple{Type{S}, Val{S}, Val{S}} where S<:Val{A} where A
    @test typeintersect(A, B) == Union{}
end
let A = Tuple{Type{T}, T, Val{T}} where T<:Val{<:Val},
    B = Tuple{Type{S}, Val{S}, Val{S}} where S<:Val
    @test_broken typeintersect(A, B) == Tuple{Type{Val{<:Val}}, Val{Val{<:Val}}, Val{Val{<:Val}}}
    @test typeintersect(A, B) <: Tuple{Type{T}, Val{T}, Val{T}} where T<:(Val{<:Val})
end
let T = Tuple{Union{Type{T}, Type{S}}, Union{Val{T}, Val{S}}, Union{Val{T}, S}} where T<:Val{A} where A where S<:Val,
    S = Tuple{Type{T}, T, Val{T}} where T<:(Val{S} where S<:Val)
    # optimal = Union{}?
    @test typeintersect(T, S) == Tuple{Type{T}, Union{Val{T}, Val{S}}, Val{T}} where {S<:Val, T<:Val}
    @test typeintersect(S, T) == Tuple{Type{T}, Union{Val{T}, Val{S}}, Val{T}} where {T<:Val, S<:(Union{Val{A}, Val} where A)}
end

#issue #49857
@test !<:(Type{Vector{Union{Base.BitInteger, Base.IEEEFloat, StridedArray, Missing, Nothing, Val{T}}}} where {T}, Type{Array{T}} where {T})

#issue 50195
let a = Tuple{Type{X} where X<:Union{Nothing, Val{X1} where {X4, X1<:(Pair{X2, Val{X2}} where X2<:Val{X4})}}},
    b = Tuple{Type{Y} where Y<:(Val{Y1} where {Y4<:Src, Y1<:(Pair{Y2, Val{Y2}} where Y2<:Union{Val{Y4}, Y4})})} where Src
    @test typeintersect(a, b) <: Any
end

#issue 50195
let a = Tuple{Union{Nothing, Type{Pair{T1}} where T1}}
    b = Tuple{Type{X2} where X2<:(Pair{T2, Y2} where {Src, Z2<:Src, Y2<:Union{Val{Z2}, Z2}})} where T2
    @test !Base.has_free_typevars(typeintersect(a, b))
end

#issue 53366
let Y = Tuple{Val{T}, Val{Val{T}}} where T
    A = Val{Val{T}} where T
    T = TypeVar(:T, UnionAll(A.var, Val{A.var}))
    B = UnionAll(T, Val{T})
    X = Tuple{A, B}
    @testintersect(X, Y, !Union{})
end

#issue 53621 (requires assertions enabled)
abstract type A53621{T, R, C, U} <: AbstractSet{Union{C, U}} end
struct T53621{T, R<:Real, C, U} <: A53621{T, R, C, U} end
let
    U = TypeVar(:U)
    C = TypeVar(:C)
    T = TypeVar(:T)
    R = TypeVar(:R)
    CC = TypeVar(:CC, Union{C, U})
    UU = TypeVar(:UU, Union{C, U})
    S1 = UnionAll(T, UnionAll(R, Type{UnionAll(C, UnionAll(U, T53621{T, R, C, U}))}))
    S2 = UnionAll(C, UnionAll(U, UnionAll(CC, UnionAll(UU, UnionAll(T, UnionAll(R, T53621{T, R, CC, UU}))))))
    S = Tuple{S1, S2}
    T = Tuple{Type{T53621{T, R}}, AbstractSet{T}} where {T, R}
    @testintersect(S, T, !Union{})
end

#issue 53371
struct T53371{A,B,C,D,E} end
S53371{A} = Union{Int, <:A}
R53371{A} = Val{V} where V<:(T53371{B,C,D,E,F} where {B<:Val{A}, C<:S53371{B}, D<:S53371{B}, E<:S53371{B}, F<:S53371{B}})
let S = Type{T53371{A, B, C, D, E}} where {A, B<:R53371{A}, C<:R53371{A}, D<:R53371{A}, E<:R53371{A}},
    T = Type{T53371{A, B, C, D, E} where {A, B<:R53371{A}, C<:R53371{A}, D<:R53371{A}, E<:R53371{A}}}
    @test !(S <: T)
end

#issue 54356
let S = Tuple{Val{Val{Union{Val{A2}, A2}}}, Val{Val{Union{Val{A2}, Val{A4}, A4}}}} where {A2, A4<:Union{Val{A2}, A2}},
    T = Tuple{Vararg{Val{V}}} where {V}
    @testintersect(S, T, !Union{})
end

#issue 54356
abstract type A54356{T<:Real} end
struct B54356{T} <: A54356{T} end
struct C54356{S,T<:Union{S,Complex{S}}} end
struct D54356{S<:Real,T} end
let S = Tuple{Val, Val{T}} where {T}, R = Tuple{Val{Val{T}}, Val{T}} where {T},
    SS = Tuple{Val, Val{T}, Val{T}} where {T}, RR = Tuple{Val{Val{T}}, Val{T}, Val{T}} where {T}
    # parameters check for self
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, Complex{B}}}, S{1}, R{1})
    # parameters check for supertype (B54356 -> A54356)
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, B54356{B}}}, S{1}, R{1})
    # enure unused TypeVar skips the `UnionAll` wrapping
    @testintersect(Tuple{Val{A}, A} where {B, A<:(Union{Val{B}, D54356{B,C}} where {C})}, S{1}, R{1})
    # invariant parameter should not get narrowed
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, Val{Union{Int,Complex{B}}}}}, S{1}, R{1})
    # bit value could not be `Union` element
    @testintersect(Tuple{Val{A}, A, Val{B}} where {B, A<:Union{B, Val{B}}}, SS{1}, RR{1})
    @testintersect(Tuple{Val{A}, A, Val{B}} where {B, A<:Union{B, Complex{B}}}, SS{1}, Union{})
    # `check_datatype_parameters` should ignore bad `Union` elements in constraint's ub
    T = Tuple{Val{Union{Val{Nothing}, Val{C54356{V,V}}}}, Val{Nothing}} where {Nothing<:V<:Nothing}
    @test T <: S{Nothing}
    @test T <: Tuple{Val{A}, A} where {B, C, A<:Union{Val{B}, Val{C54356{B,C}}}}
    @test T <: typeintersect(Tuple{Val{A}, A} where {B, C, A<:Union{Val{B}, Val{C54356{B,C}}}}, S{Nothing})
    # extra check for Vararg
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, NTuple{B,Any}}}, S{-1}, R{-1})
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, Tuple{Any,Vararg{Any,B}}}}, S{-1}, R{-1})
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, Tuple{Vararg{Int,Union{Int,Complex{B}}}}}}, S{1}, R{1})
    # extra check for NamedTuple
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, NamedTuple{B,Tuple{Int}}}}, S{1}, R{1})
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, NamedTuple{B,Tuple{Int}}}}, S{(1,)}, R{(1,)})
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, NamedTuple{(:a),B}}}, S{NTuple{2,Int}}, R{NTuple{2,Int}})
    @testintersect(Tuple{Val{A}, A} where {B, A<:Union{Val{B}, NamedTuple{B,Tuple{Int,Int}}}}, S{(:a,:a)}, R{(:a,:a)})
end

#issue 56040
let S = Dict{V,V} where {V},
    T = Dict{Ref{Union{Set{A2}, Set{A3}, A3}}, Ref{Union{Set{A3}, Set{A2}, Set{A1}, Set{A4}, A4}}} where {A1, A2<:Set{A1}, A3<:Union{Set{A1}, Set{A2}}, A4<:Union{Set{A2}, Set{A1}, Set{A3}}},
    A = Dict{Ref{Set{Union{}}}, Ref{Set{Union{}}}}
    @testintersect(S, T, !Union{})
    @test A <: typeintersect(S, T)
    @test A <: typeintersect(T, S)
end

#issue 56606
let
    A = Tuple{Val{1}}
    B = Tuple{Val}
    for _ in 1:30
        A = Tuple{Val{A}}
        B = Tuple{Val{<:B}}
    end
    @test A <: B
end
@testintersect(
    Val{Tuple{Int,S,T}} where {S<:Any,T<:Vector{Vector{Int}}},
    Val{Tuple{T,R,S}} where {T,R<:Vector{T},S<:Vector{R}},
    Val{Tuple{Int, Vector{Int}, T}} where T<:Vector{Vector{Int}},
)

#issue 57429
@testintersect(
    Pair{<:Any, <:Tuple{Int}},
    Pair{N, S} where {N, NTuple{N,Int}<:S<:NTuple{M,Int} where {M}},
    !Union{}
)
@testintersect(
    Pair{N, T} where {N,NTuple{N,Int}<:T<:NTuple{N,Int}},
    Pair{N, T} where {N,NTuple{N,Int}<:T<:Tuple{Int,Vararg{Int}}},
    !Union{}
)
