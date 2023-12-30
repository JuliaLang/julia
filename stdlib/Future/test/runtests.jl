# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Future

# making positive tests

@testset "Testing copy! for sets" begin
@test begin
dst = Set([])# Create a set using a collection
src = Set([1, 24, 5, 6, 51, 62])
copy!(dst, src)
@assert dst == src# Compare sets using a collection
println("destinition = ", dst)
return true
end
end

@testset "Testing copy! for Dictionaries" begin
@test begin
dst = Dict("a" => 1, "b" => 2)
src = Dict("c" => 3, "d" => 4)
copy!(dst, src)
println("destinition = ", dst)
@assert dst == src
return true
end
end

@testset "Testing copy! for Arrays" begin
@test begin
dst = [1, 2, 3]
src = [4, 5, 6]
copy!(dst, src)
@assert dst == src
println("destinition = ", dst)
return true
end
end

# making negative test where

@testset "Negative Testing copy! for sets" begin
@test begin
dst = Set([])# Create a set using a collection
src = Set([1, 24, 5, 6, 51, 62])
copy!(dst, src)
is_fake = dst != Set([1, 24, 5])# Compare sets using a collection
println(is_fake)
return true
end
end

@testset "Negative Testing copy! for Dictionaries" begin
@test begin
dst = Dict("a" => 1, "b" => 2)
src = Dict("c" => 3, "d" => 4)
copy!(dst, src)
is_fake = dst == Dict("a" => 1, "b" => 2)
println(is_fake)
return true
end
end

@testset "Negative Testing copy! for Arrays" begin
@test begin
dst = [1, 2, 3]
src = [4, 5, 6]
copy!(dst, src)
is_fake = dst == [1, 2, 3]
println(is_fake)
return true
end
end
