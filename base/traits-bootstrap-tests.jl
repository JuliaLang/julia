# Tests which can be run during bootstrap.  Essentially the same as in test/traits.jl

println("--------------------trait-----------------")
println("Running traits tests:")
### tests
@traitdef Tr{X}
@traitimpl Tr{Int}
@traitfn ff985{X; Tr{X}}(x::X) = x
a = ff985(5)
# println("Running traitfn:")
# println(ff985(5))

## run tests

macro assert_(ex)
    :($(esc(ex)) ? $(nothing) : (print("------------- Error in:  "); println($(esc(ex)))) )
end

@traitdef Tr1{X}
@assert_ trait(Tr1{Int})==Not{Tr1{Int}}
@assert_ !istrait(Tr1{Int})
@traitimpl Tr1{Integer}
@assert_ trait(Tr1{Int})==Tr1{Int}
@assert_ istrait(Tr1{Int})
@assert_ trait(Tr1{Bool})==Tr1{Bool}

# Logic.  trait(Tr) returns the same trait Tr if it is fulfilled and
# Not{Tr} otherwise.  This is a bit confusing.
@assert_ trait(Tr1{AbstractString})==Not{Tr1{AbstractString}}
@assert_ istrait(Tr1{AbstractString})==false
@assert_ trait(Not{Tr1{AbstractString}})==Not{Tr1{AbstractString}}
@assert_ istrait(Not{Tr1{AbstractString}})==true
@assert_ trait(Not{Not{Tr1{AbstractString}}})==Not{Tr1{AbstractString}}
@assert_ istrait(Not{Not{Tr1{AbstractString}}})==false
@assert_ trait(Not{Not{Not{Tr1{AbstractString}}}})==Not{Tr1{AbstractString}}
@assert_ istrait(Not{Not{Not{Tr1{AbstractString}}}})==true

@assert_ trait(Not{Tr1{Integer}})==Tr1{Integer}
@assert_ istrait(Not{Tr1{Integer}})==false
@assert_ trait(Not{Not{Tr1{Integer}}})==Tr1{Integer}
@assert_ istrait(Not{Not{Tr1{Integer}}})==true
@assert_ trait(Not{Not{Not{Tr1{Integer}}}})==Tr1{Integer}
@assert_ istrait(Not{Not{Not{Tr1{Integer}}}})==false

@traitdef Tr2{X,Y}
@assert_ trait(Tr2{Int,AbstractFloat})==Not{Tr2{Int,AbstractFloat}}
@traitimpl Tr2{Integer, Float64}
@assert_ trait(Tr2{Int, Float64})==Tr2{Int, Float64}
@assert_ trait(Tr2{Int, Float32})==Not{Tr2{Int, Float32}}

# trait functions
@traitfn f985{X; Tr1{X}}(x::X) = 1  # def 1
@traitfn f985{X; !Tr1{X}}(x::X) = 2
@assert_ f985(5)==1
@assert_ f985(5.)==2


@traitfn f985{X,Y; Tr2{X,Y}}(x::X,y::Y,z) = 1
@assert_ f985(5,5., "a")==1
@traitfn f985{X,Y; !Tr2{X,Y}}(x::X,y::Y,z) = 2
@assert_ f985(5,5, "a")==2
# This will overwrite the definition def1 above
@traitfn f985{X; !Tr2{X,X}}(x::X) = 10
@traitfn f985{X; Tr2{X,X}}(x::X) = 100
@assert_ f985(5)==10
@assert_ f985(5.)==10
@traitimpl Tr2{Integer, Integer}
@assert_ f985(5.)==10
println("This fails on the first round of coreimg.jl:")
@assert_ f985(5)==10
println("end of failure")
# need to update method cache to make above right:
@traitfn f985{X; Tr2{X,X}}(x::X) = 100
@assert_ f985(5)==100
@assert_ f985(5.)==10

# VarArg
@traitfn g{X; Tr1{X}}(x::X, y...) = y
@assert_ g(5, 7, 8)==((7,8),)
### @assert_ g(5.0, 7, 8)==((7,8),) # hangs because of https://github.com/JuliaLang/julia/issues/13183
## With macros
@traitfn @generated ggg{X; Tr1{X}}(x::X) = ( x<:Array ? :(x[1]+1) : :(x))
#@traitfn @generated ggg{X; Tr1{X}}(x::X) = ( println(x); x<:Array ? :(x[1]+1) : :(x))
@assert_ ggg(5)==5
@traitimpl Tr1{AbstractArray}
a = Array(Any,1) # Array(Int,1) does not work yet!?
a[1] = 5
@assert_ ggg(a)==6

# traitfn with Type
@traitfn ggt{X; Tr1{X}}(::Type{X}, y) = (X,y)
@assert_ ggt(Array, 5)==(Array, 5)

# traitfn with ::X
@traitfn gg27{X; Tr1{X}}(::X) = X
@assert_ gg27(a)==Array{Int,1}

println("Traits tests done")
