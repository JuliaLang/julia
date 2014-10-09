using Compat
using Base.Test

d = Dict{Int,Int}()
d[1] = 1
@test Compat.@Dict(1 => 1) == d

ad = Dict{Any,Any}()
ad[1] = 1
@test Compat.@AnyDict(1 => 1) == ad
