#!/usr/bin/env -S julia --project=@scriptdir

module Main2

Base.@ccallable function main() :: Cint

    # Basic interpolation
    println(Core.stdout, "Hello, world! $(rand(UInt))")

    # Interpolate many (homogeneous) arguments
    println(Core.stdout, "\nUInt interpolation tests:")
    println(Core.stdout, " 1 interpolant : $(rand(UInt))...")
    println(Core.stdout, " 2 interpolants: $(rand(UInt)) $(rand(UInt))...")
    println(Core.stdout, " 3 interpolants: $(rand(UInt)) $(rand(UInt)) $(rand(UInt))...")
    println(Core.stdout, " 4 interpolants: $(rand(UInt)) $(rand(UInt)) $(rand(UInt)) $(rand(UInt))...")
    # println(Core.stdout, " 5 interpolants: $(rand(UInt)) $(rand(UInt)) $(rand(UInt)) $(rand(UInt)) $(rand(UInt))...") # broken

    # Interpolate many (heterogeneous) arguments
    println(Core.stdout, "\nInteger interpolation tests:")
    println(Core.stdout, " 1 interpolant : $(rand(UInt128))...")
    println(Core.stdout, " 2 interpolants: $(rand(UInt128)) $(rand(UInt64))...")
    println(Core.stdout, " 3 interpolants: $(rand(UInt128)) $(rand(UInt64)) $(rand(UInt32))...")
    # println(Core.stdout, " 4 interpolants: $(rand(UInt128)) $(rand(UInt64)) $(rand(UInt32)) $(rand(UInt16))...") # broken
    # println(Core.stdout, " 5 interpolants: $(rand(UInt128)) $(rand(UInt64)) $(rand(UInt32)) $(rand(UInt16)) $(rand(UInt8))...") # broken

    return 0
end

end
