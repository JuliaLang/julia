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

    @test !issub(Tuple{Val{3}, Vararg{Val{3}}}, Tuple{Vararg{Val{N}, N} where N})

    @test issub_strict(Tuple{Int,Int}, Tuple{Int,Int,Vararg{Int,N}} where N)
    @test issub_strict(Tuple{Int,Int}, Tuple{E,E,Vararg{E,N}} where E where N)

    @test issub(Type{Tuple{VecElement{Bool}}}, (Type{Tuple{Vararg{VecElement{T},N}}} where T where N))

    @test isequal_type(Type{Tuple{Vararg{Int,N}} where N}, Type{Tuple{Vararg{Int,N} where N}})
    @test Type{Tuple{Vararg{Int,N}} where N} !== Type{Tuple{Vararg{Int,N} where N}}
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
    @test_broken issub(Ref{Tuple{T} where T},        Ref{Tuple{T}} where T)
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

const menagerie =
    Any[Bottom, Any, Int, Int8, Integer, Real,
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
        (@UnionAll T Union{Tuple{T,Array{T,1}}, Tuple{T,Array{Int,1}}}),
        ]

let new = Any[]
    # add variants of each type
    for T in menagerie
        push!(new, Ref{T})
        push!(new, Tuple{T})
        push!(new, Tuple{T,T})
        push!(new, Tuple{Vararg{T}})
        push!(new, @UnionAll S<:T S)
        push!(new, @UnionAll S<:T Ref{S})
    end
    append!(menagerie, new)
end

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
            @test issubTS == issub(Tuple{T}, Tuple{S})
            @test issubTS == issub(Tuple{Vararg{T}}, Tuple{Vararg{S}})
            @test issubTS == issub(Tuple{T}, Tuple{Vararg{S}})

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

    @testintersect((@UnionAll S Tuple{S,Vector{S}}), (@UnionAll T<:Real Tuple{T,AbstractVector{T}}),
                   (@UnionAll S<:Real Tuple{S,Vector{S}}))

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

    #@test isequal_type(typeintersect((@UnionAll N Tuple{NTuple{N,Any},Array{Int,N}}),
    #                                 Tuple{Tuple{Int,Vararg{Int}},Array}),
    #                   Tuple{Tuple{Int,Vararg{Int}},Array{Int,N}})

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

    @testintersect(Tuple{Array{Any,1}, Tuple{Int64, Int64, Vararg{Int64, N} where N}},
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
    #               Union{Tuple{T,T} where T<:UpperTriangular,
    #                     Tuple{T,T} where T<:UnitUpperTriangular})
    @testintersect(Tuple{T,T} where T<:Union{UpperTriangular, UnitUpperTriangular},
                   Tuple{AbstractArray{T,N}, AbstractArray{T,N}} where N where T,
                   Tuple{T,T} where T<:Union{UpperTriangular, UnitUpperTriangular})

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

    @testintersect(Tuple{Type{S}, Tuple{Any, Vararg{Any}}} where S<:Tuple{Any, Vararg{Any}},
                   Tuple{Type{T}, T} where T,
                   Tuple{Type{S},S} where S<:Tuple{Any,Vararg{Any,N} where N})

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
    @testintersect(Tuple{Type{Tuple{Vararg{T, N} where N}}, Tuple} where T,
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
                   Tuple{Type{Tuple{Vararg{Integer,N} where N}},Tuple{Vararg{Integer,N} where N}})
    @testintersect(Tuple{Type{Tuple{Vararg{Union{Int,Symbol}}}}, Tuple},
                   Tuple{Type{Tuple{Vararg{V}}}, Tuple{Vararg{V}}} where {V},
                   Tuple{Type{Tuple{Vararg{Union{Int,Symbol},N} where N}},Tuple{Vararg{Union{Int,Symbol},N} where N}})

    # non types
    @testintersect(Tuple{1}, Tuple{Any}, Tuple{1})

    # tests for robustness after incorrect datatype allocation normalization
    @test typeintersect(Vector{Tuple{T, T} where Number<:T<:Number}, Vector{Tuple{Number, Number}}) === Vector{Tuple{T, T} where Number<:T<:Number}
    @test typeintersect(Vector{Tuple{Number, Number}}, Vector{Tuple{T, T} where Number<:T<:Number}) === Vector{Tuple{Number, Number}}
end

function test_intersection_properties()
    for T in menagerie
        for S in menagerie
            I = _type_intersect(T,S)
            I2 = _type_intersect(S,T)
            @test isequal_type(I, I2)
            @test issub(I, T) && issub(I, S)
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
    @test typeintersect(a, b) <: b
end
let a = Tuple{T1,T1} where T1,
    b = Tuple{Val{S2},S6} where S2 where S6
    @test typeintersect(a, b) == typeintersect(b, a)
end
let a = Val{Tuple{T1,T1}} where T1,
    b = Val{Tuple{Val{S2},S6}} where S2 where S6
    @testintersect(a, b, Val{Tuple{Val{T},Val{T}}} where T)
end
let a = Tuple{Float64,T3,T4} where T4 where T3,
    b = Tuple{S2,Tuple{S3},S3} where S2 where S3
    @test typeintersect(a, b) == typeintersect(b, a)
end
let a = Tuple{T1,Tuple{T1}} where T1,
    b = Tuple{Float64,S3} where S3
    @test typeintersect(a, b) <: a
end
let a = Tuple{5,T4,T5} where T4 where T5,
    b = Tuple{S2,S3,Tuple{S3}} where S2 where S3
    @test typeintersect(a, b) == typeintersect(b, a)
end
let a = Tuple{T2,Tuple{T4,T2}} where T4 where T2,
    b = Tuple{Float64,Tuple{Tuple{S3},S3}} where S3
    @test typeintersect(a, b) <: b
end
let a = Tuple{Tuple{T2,4},T6} where T2 where T6,
    b = Tuple{Tuple{S2,S3},Tuple{S2}} where S2 where S3
    @test typeintersect(a, b) == typeintersect(b, a)
end
let a = Tuple{T3,Int64,Tuple{T3}} where T3,
    b = Tuple{S3,S3,S4} where S4 where S3
    @test_broken typeintersect(a, b) <: a
end
let a = Tuple{T1,Val{T2},T2} where T2 where T1,
    b = Tuple{Float64,S1,S2} where S2 where S1
    @test typeintersect(a, b) == typeintersect(b, a)
end
let a = Tuple{T1,Val{T2},T2} where T2 where T1,
    b = Tuple{Float64,S1,S2} where S2 where S1
    @test_broken typeintersect(a, b) <: a
end
let a = Tuple{Float64,T1} where T1,
    b = Tuple{S1,Tuple{S1}} where S1
    @test typeintersect(a, b) <: b
end
let a = Tuple{Val{T1},T2,T2} where T2 where T1,
    b = Tuple{Val{Tuple{S2}},S3,Float64} where S2 where S3
    @testintersect(a, b, Tuple{Val{Tuple{S2}},Float64,Float64} where S2)
end
let a = Tuple{T1,T2,T2} where T1 where T2,
    b = Tuple{Val{S2},S2,Float64} where S2,
    x = Tuple{Val{Float64},Float64,Float64}
    @test x <: typeintersect(a, b)
end
let a = Val{Tuple{T1,Val{T2},Val{Int64},Tuple{Tuple{T3,5,Float64},T4,T2,T5}}} where T1 where T5 where T4 where T3 where T2,
    b = Val{Tuple{Tuple{S1,5,Float64},Val{S2},S3,Tuple{Tuple{Val{Float64},5,Float64},2,Float64,S4}}} where S2 where S3 where S1 where S4
    @test_skip typeintersect(b, a)
end

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
    @test isa(ex, ErrorException) && ex.msg == "invalid subtyping in definition of A19414"
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
@test [Tuple{Vararg{Int64,N} where N}, Tuple{Vararg{Int64,N}} where N] isa Vector{Type}
f24521(::Type{T}, ::Type{T}) where {T} = T
@test f24521(Tuple{Any}, Tuple{T} where T) == Tuple{Any}
@test f24521(Tuple{Vararg{Int64,N} where N}, Tuple{Vararg{Int64,N}} where N) == Tuple{Vararg{Int64,N}} where N

# issue #26654
@test !(Ref{Union{Int64, Ref{Number}}} <: Ref{Union{Ref{T}, T}} where T)
@test !(Ref{Union{Int64, Val{Number}}} <: Ref{Union{Val{T}, T}} where T)
@test !(Ref{Union{Ref{Number}, Int64}} <: Ref{Union{Ref{T}, T}} where T)
@test !(Ref{Union{Val{Number}, Int64}} <: Ref{Union{Val{T}, T}} where T)

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
                     TM} where TM<:M29955{Float64,Array{Float64,1}})
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

#@testintersect(Tuple{Any,Tuple{Int},Int},
#               Tuple{LT,R,I} where LT<:Union{I, R} where R<:Tuple{I} where I<:Integer,
#               Tuple{LT,Tuple{Int},Int} where LT<:Union{Tuple{Int},Int})
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
@testintersect(Tuple{Pair{B,C},Union{C,Pair{B,C}},Union{B,Real}} where {B,C},
               Tuple{Pair{B,C},C,C} where {B,C},
               Tuple{Pair{B,C},C,C} where C<:Union{Real, B} where B)
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
               Tuple{Type{Val{T}},Int,T} where T<:Integer)
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
               Pair{T, Ref{Tuple{Ref{Ref{Tuple{Int}}},Ref{Float64}}}} where T)
# TODO: should be able to get this result
#              Pair{Float64, Ref{Tuple{Ref{Ref{Tuple{Int}}},Ref{Float64}}}}

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
               # TODO: this could be narrower
               Tuple{Type{SA{2, L}}, Type{SA{2, 16}}} where L)

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
    T3 = Tuple{Int, Any, Any, Any, Integer, Vararg{Integer, N} where N}

    @test issub_strict(T1, T2)
    @test issub_strict(T2, T3)
    @test issub_strict(T1, T3)
end
let A = Tuple{Float64, Vararg{Int64, 2}},
    B1 = Tuple{Float64, Vararg{T, 2}} where T <: Int64,
    B2 = Tuple{Float64, T, T} where T <: Int64,
    C = Tuple{Float64, Any, Vararg{Integer, N} where N}

    @test A == B1 == B2
    @test issub_strict(A, C)
    @test issub_strict(B1, C)
    @test issub_strict(B2, C)
end
let A = Tuple{Vararg{Val{N}, N} where N},
    B = Tuple{Vararg{Val{N}, N}} where N,
    C = Tuple{Val{2}, Val{2}}

    @test isequal_type(A, B)
    @test issub(C, B)
    @test issub(C, A)
end
@test isequal_type(Tuple{T, Vararg{T, 2}} where T<:Real, Tuple{Vararg{T, 3}} where T<: Real)
@test !issub(Tuple{Vararg{T, 3}} where T<:Real, Tuple{Any, Any, Any, Any, Vararg{Any, N} where N})
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

# this is is a timing test, so it would fail on debug builds
#let T = Type{Tuple{(Union{Int, Nothing} for i = 1:23)..., Union{String, Nothing}}},
#    S = Type{T} where T<:Tuple{E, Vararg{E}} where E
#    @test @elapsed (@test T != S) < 5
#end

# issue #32386
@test typeintersect(Type{S} where S<:(Vector{Pair{_A,N} where N} where _A),
                    Type{Vector{T}} where T) == Type{Vector{Pair{_A,N} where N}} where _A

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
@test_broken typeintersect(Tuple{Vector{Vector{Float32}},Matrix,Matrix},
                           Tuple{Vector{V},Matrix{Int},Matrix{S}} where {S, V<:AbstractVector{S}}) ==
             Tuple{Array{Array{Float32,1},1},Array{Int,2},Array{Float32,2}}

@testintersect(Tuple{Pair{Int, DataType}, Any},
               Tuple{Pair{A, B} where B<:Type, Int} where A,
               Tuple{Pair{Int, DataType}, Int})

# issue #33337
@test !issub(Tuple{Type{T}, T} where T<:NTuple{30, Union{Nothing, Ref}},
             Tuple{Type{Tuple{Vararg{V, N} where N}}, Tuple{Vararg{V, N} where N}} where V)
@test  issub(Tuple{Type{Any}, NTuple{4,Union{Int,Nothing}}},
             Tuple{Type{V}, Tuple{Vararg{V, N} where N}} where V)

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
             Tuple{Type{Tuple{Vararg{V, N} where N}}, Tuple{Vararg{V, N} where N}} where V)

# issue 36100
@test NamedTuple{(:a, :b), Tuple{Missing, Union{}}} == NamedTuple{(:a, :b), Tuple{Missing, Union{}}}
@test Val{Tuple{Missing, Union{}}} === Val{Tuple{Missing, Union{}}}
