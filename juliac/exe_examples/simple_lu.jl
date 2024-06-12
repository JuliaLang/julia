#!/usr/bin/env -S julia --project=@scriptdir

module Main2
using LinearAlgebra

Base.@ccallable function main()::Cint
    println(Core.stdout, "Hello, world!")
    A = rand(10, 10)
    L = lu(A)
    ccall(:jl_, Cvoid, (Any,), L)
    return 0
end

end
