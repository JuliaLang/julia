#!/usr/bin/env -S julia --project=@scriptdir

module Main2
using LinearAlgebra

Base.@ccallable function main()::Cint
    println(Core.stdout, "Hello, world!")
    A = rand(10, 10)
    B = rand(10)
    L = mapreduce(sin, +, A)
    L2 = mapreduce(sin, +, A, dims=1)
    L3 = mapreduce(sin, +, B)
    L4 = minimum(A)
    ccall(:jl_, Cvoid, (Any,), L)
    ccall(:jl_, Cvoid, (Any,), L2)
    ccall(:jl_, Cvoid, (Any,), L3)
    ccall(:jl_, Cvoid, (Any,), L4)
    return 0
end

end
