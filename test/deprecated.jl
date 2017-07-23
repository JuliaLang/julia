# Once we drop 0.4 support, we want the following to return depwarns
# info("Beginning of tests with deprecation warnings")

v = 1
d = Dict{Int,Int}()
d[1] = 1
@test Compat.@Dict(1 => 1) == d
@test Compat.@Dict(1 => v) == d

ad = Dict{Any,Any}()
ad[1] = 1
@test Compat.@AnyDict(1 => 1) == ad
@test Compat.@AnyDict(1 => v) == ad

module CartesianTest
    using Base.Cartesian, Compat
    @ngenerate N NTuple{N,Int} function f(X::NTuple{N,Int}...)
        @ncall N tuple X
    end
end

@test CartesianTest.f(1) == (1,)
@test CartesianTest.f(1,2) == (1,2)
@test CartesianTest.f(1,2,3) == (1,2,3)
@test CartesianTest.f(1,2,3,4) == (1,2,3,4)
@test CartesianTest.f(1,2,3,4,5) == (1,2,3,4,5)

@test Compat.KERNEL == Sys.KERNEL

@test String == @compat(Union{Compat.UTF8String,Compat.ASCIIString})

@test isa("hello", Compat.ASCIIString)
@test isa("λ", Compat.UTF8String)

# @functorize
function checkfunc(Fun, func)
    @eval @test @functorize($(func)) === Base.$(func)
end

for (Fun, func) in [(:IdFun,                   :identity),
                    (:AbsFun,                  :abs),
                    (:Abs2Fun,                 :abs2),
                    (:ExpFun,                  :exp),
                    (:LogFun,                  :log),
                    (:ConjFun,                 :conj)]
    begin
        if isdefined(Base, func)
            checkfunc(Fun, func)
            a = rand(1:10, 10)
            @eval @test mapreduce($(func), +, $(a)) == mapreduce(@functorize($(func)), +, $(a))
        end
    end
end

dotfunctors = [(:DotAddFun,           :.+),
               (:DotSubFun,           :.-),
               (:DotMulFun,           :.*),
               (:DotRDivFun,          :./),
               (:DotIDivFun,          Symbol(".÷")),
               (:DotRemFun,           :.%),
               (:DotLSFun,            :.<<),
               (:DotRSFun,            :.>>)]

functors    = [(:AndFun,              :&),
               (:OrFun,               :|),
               (:XorFun,              :⊻),
               (:AddFun,              :+),
               (:SubFun,              :-),
               (:MulFun,              :*),
               (:RDivFun,             :/),
               (:LDivFun,             :\ ),
               (:IDivFun,             :div),
               (:ModFun,              :mod),
               (:RemFun,              :rem),
               (:PowFun,              :^),
               (:MaxFun,              :scalarmax),
               (:MinFun,              :scalarmin),
               (:LessFun,             :<),
               (:MoreFun,             :>),
               (:ElementwiseMaxFun,   :max),
               (:ElementwiseMinFun,   :min)]

# since #17623, dot functions are no longer function objects
append!(functors, dotfunctors)

for (Fun, func) in functors
    begin
        if isdefined(Base, func)
            checkfunc(Fun, func)
            a = rand(1:10, 10)
            @eval @test mapreduce(identity, Base.$(func), $(a)) == mapreduce(identity, @functorize($(func)), $(a))
        end
    end
end

@test @functorize(complex) === complex
@test @functorize(dot) === dot
let a = rand(1:10, 10)
    @test mapreduce(identity, dot, a) == mapreduce(identity, @functorize(dot), a)
end

@test isa(@functorize(centralizedabs2fun)(1), @functorize(centralizedabs2fun))
@test isa(@functorize(centralizedabs2fun)(1.0), @functorize(centralizedabs2fun))
let a = rand(1:10, 10)
    @eval @test mapreduce(x -> abs2(x - 1), +, $(a)) == mapreduce(@functorize(centralizedabs2fun)(1), +, $(a))
end

@test Compat.promote_eltype_op(@functorize(+), ones(2,2), 1) === Float64
@test Compat.promote_eltype_op(@functorize(*), ones(Int, 2), zeros(Int16,2)) === Int
