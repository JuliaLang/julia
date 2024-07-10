#!/usr/bin/env -S julia --project=@scriptdir

module Main2
#using LinearAlgebra

Base.@ccallable function main()::Cint
    #println(Core.stdout, "Hello, world!")
    #A = rand(10, 10)
    #L = sin.(A)
    B = rand(10)
    L2 = sin.(B)
    #ccall(:jl_, Cvoid, (Any,), L)
    #ccall(:jl_, Cvoid, (Any,), L2)
    return 0
end

end
