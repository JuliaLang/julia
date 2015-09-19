module TraitTests
using Base.Test

# @test_throws MethodError trait(4)
@test_throws ErrorException istrait(4)

# definition & adding
@traitdef Tr1{X}
@test trait(Tr1{Int})==Not{Tr1{Int}}
@test !istrait(Tr1{Int})
@traitimpl Tr1{Integer}
@test trait(Tr1{Int})==Tr1{Int}
@test istrait(Tr1{Int})
@test trait(Tr1{Bool})==Tr1{Bool}
@test trait(Tr1{AbstractString})==Not{Tr1{AbstractString}}
@test !istrait(Tr1{AbstractString})

# Logic.  trait(Tr) returns the same trait Tr if it is fulfilled and
# Not{Tr} otherwise.  This is a bit confusing.
@test trait(Tr1{AbstractString})==Not{Tr1{AbstractString}}
@test istrait(Tr1{AbstractString})==false
@test trait(Not{Tr1{AbstractString}})==Not{Tr1{AbstractString}}
@test istrait(Not{Tr1{AbstractString}})==true
@test trait(Not{Not{Tr1{AbstractString}}})==Not{Tr1{AbstractString}}
@test istrait(Not{Not{Tr1{AbstractString}}})==false
@test trait(Not{Not{Not{Tr1{AbstractString}}}})==Not{Tr1{AbstractString}}
@test istrait(Not{Not{Not{Tr1{AbstractString}}}})==true

@test trait(Not{Tr1{Integer}})==Tr1{Integer}
@test istrait(Not{Tr1{Integer}})==false
@test trait(Not{Not{Tr1{Integer}}})==Tr1{Integer}
@test istrait(Not{Not{Tr1{Integer}}})==true
@test trait(Not{Not{Not{Tr1{Integer}}}})==Tr1{Integer}
@test istrait(Not{Not{Not{Tr1{Integer}}}})==false


@traitdef Tr2{X,Y}
@test trait(Tr2{Int,AbstractFloat})==Not{Tr2{Int,AbstractFloat}}
@traitimpl Tr2{Integer, Float64}
@test trait(Tr2{Int, Float64})==Tr2{Int, Float64}
@test trait(Tr2{Int, Float32})==Not{Tr2{Int, Float32}}

# trait functions
@traitfn f{X; Tr1{X}}(x::X) = 1  # def 1
@traitfn f{X; !Tr1{X}}(x::X) = 2
@test f(5)==1
@test f(5.)==2

@traitfn f{X,Y; Tr2{X,Y}}(x::X,y::Y,z) = 1
@test f(5,5., "a")==1
@test_throws MethodError f(5,5, "a")==2
@traitfn f{X,Y; !Tr2{X,Y}}(x::X,y::Y,z) = 2
@test f(5,5, "a")==2

# This will overwrite the definition def1 above
@traitfn f{X; !Tr2{X,X}}(x::X) = 10
@traitfn f{X; Tr2{X,X}}(x::X) = 100
@test f(5)==10
@test f(5.)==10
@traitimpl Tr2{Integer, Integer}
@test f(5.)==10
@test !(f(5)==100)
# need to update method cache:
@traitfn f{X; Tr2{X,X}}(x::X) = 100
@test f(5)==100
@test f(5.)==10

# VarArg
@traitfn g{X; Tr1{X}}(x::X, y...) = y
@test g(5, 7, 8)==((7,8),)
# @test g(5.0, 7, 8)==((7,8),) # hangs because of https://github.com/JuliaLang/julia/issues/13183
@traitfn g{X; !Tr1{X}}(x::X, y...) = 99
@test g(5.0, 7, 8)==99

## With macros
@traitfn @generated ggg{X; Tr1{X}}(x::X) = ( x<:Array ? :(x[1]+1) : :(x))
#@traitfn @generated ggg{X; Tr1{X}}(x::X) = ( println(x); x<:Array ? :(x[1]+1) : :(x))
@test ggg(5)==5
@traitimpl Tr1{AbstractArray}
a = Array(Any,1) # Array(Int,1) does not work yet!?
a[1] = 5
@test ggg(a)==6

# traitfn with Type
@traitfn ggt{X; Tr1{X}}(::Type{X}, y) = (X,y)
@test ggt(Array, 5)==(Array, 5)

######
# Other tests
#####
@test istrait(IsAnything{Any})
@test istrait(IsAnything{Union{}})
@test istrait(IsAnything{Int})

@test !istrait(IsNothing{Any})
@test !istrait(IsNothing{Union{}})
@test !istrait(IsNothing{Int})

@test !istrait(IsCallable{Float64})
@test istrait(IsCallable{Function})

## TODO: activate once these are done
# @test istrait(IsBits{Int})
# @test !istrait(IsBits{Vector{Int}})

# @test istrait(IsImmutable{Float64})
# @test !istrait(IsImmutable{Vector{Int}})

# if VERSION>v"0.4-" # use @generated functions
#     @test istrait(IsContiguous{SubArray{Int64,1,Array{Int64,1},Tuple{UnitRange{Int64}},1}})
#     @test !istrait(IsContiguous{SubArray{Int64,1,Array{Int64,1},Tuple{StepRange{Int64,Int64}},1}})

#     @test istrait(IsFastLinearIndex{Vector})
#     @test !istrait(IsFastLinearIndex{AbstractArray})

#     @test istrait(IsCallable{Base.AddFun})
# end
end
